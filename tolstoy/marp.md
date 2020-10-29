
<style>
  .slide {
    font-size: 23px !important;
  }
</style>

# Какие проблемы решаем

* Очень сложный и тупой код с крудами
  * Да, это частично решается с помощью `pg-schema`
* Остается проблема истории
* Проблема размазанности логики изменения документов по коду
* Документы вообще не надо размазывать по таблицам
* Однако наивный подход с `jsonb` полем не работает. Можем записать и не прочесть после изменения кода.

---

# Как

* Храним документ `jsonb` полем
* Документы изменяются чистой функцией `docAction :: Document -> ActionData -> Document`
* Рядом с документом храним данные для функции `docAction` тоже в виде `jsonb`
* Используем автоматические миграции 
* Храним в базе метаинформацию о наших документах (как `sqitch`)

---


## Схема

```sql
CREATE TABLE revisions (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  parent_id          uuid REFERENCES ^{actions}(id) UNIQUE,
  -- The link to the parent history record.
  document           jsonb NOT NULL,
  document_version   bigint NOT NULL,
  -- The version number described in the "versions" table
  "action"           jsonb NOT NULL,
  action_version     bigint NOT NULL
);

CREATE TABLE documents (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  action_id          uuid NOT NULL REFERENCES revisions(id)
  -- The reference to the latest action of the document. The head of
  -- the liked list formed by the "parent_id" reference in the
  -- "actions" table.
);
```

---

## Немного Хаскеля

Давайте придумаем такой кайнд

```haskell
-- | Kind for describing the structure of the document
data Structure
  = StructString
  | StructNumber
  | StructBool
  | StructOptional Structure
  | StructVector Structure
  | StructSum SumTree
  | StructProduct ProductTree

data SumTree
  = Sum1 Symbol Structure
  | Sum2 SumTree SumTree

data ProductTree
  = Product0
  | Product1 Symbol Structure
  | Product2 ProductTree ProductTree
```

* Тип с данным кайндом описывает структуру хранимого документа.
* Используем его для параметризации 2х основных функторов
* Все суммы и проиведения тегированы тайплевел строчками

---

### StructureRep

<span style="font-size: 20px">
  
```haskell
data StructureRep :: Structure -> * where
  StringRep   :: StructureRep StructString
  NumberRep   :: StructureRep StructNumber
  BoolRep     :: StructureRep StructBool
  OptionalRep :: !(StructureRep s) -> StructureRep (StructOptional s)
  VectorRep   :: !(StructureRep s) -> StructureRep (StructVector s)
  SumRep      :: !(SumTreeRep t) -> StructureRep (StructSum t)
  ProductRep  :: !(ProductTreeRep t) -> StructureRep (StructProduct t)

data SumTreeRep :: SumTree -> * where
  Sum1Rep
    :: (KnownSymbol t)
    => Proxy t
    -> !(StructureRep s)
    -> SumTreeRep ('Sum1 t s)
  Sum2Rep
    :: !(SumTreeRep t1)
    -> !(SumTreeRep t2)
    -> SumTreeRep ('Sum2 t1 t2)

data ProductTreeRep :: ProductTree -> * where
  Product0Rep :: ProductTreeRep 'Product0
  Product1Rep
    :: (KnownSymbol t)
    => Proxy t
    -> !(StructureRep s)
    -> ProductTreeRep ('Product1 t s)
  Product2Rep
    :: !(ProductTreeRep t1)
    -> !(ProductTreeRep t2)
    -> ProductTreeRep ('Product2 t1 t2)
```

</span>

---

### class KnownStructure

```haskell
-- | Materialize any structure type to it's representation
class KnownStructure (s :: Structure) where
  structureRep :: StructureRep s
```

* `StructureRep` это синглтон для `Structure`
* Для всех `s`, если у нас есть `StructKind s` то для него можем получить значение `StructureRep s`
* Значение `StructureRep s` тоже сохраняется в базу!

---

### StructureValule

<span style="font-size: 16px">

```haskell
data StructureValue :: Structure -> * where
  StringValue   :: !Text -> StructureValue 'StructString
  NumberValue   :: !Scientific -> StructureValue 'StructNumber
  BoolValue     :: !Bool -> StructureValue 'StructBool
  OptionalValue
    :: !(Maybe (StructureValue s))
    -> StructureValue ('StructOptional s)
  VectorValue
    :: !(Vector (StructureValue s))
    -> StructureValue ('StructVector s)
  SumValue
    :: !(SumTreeValue t)
    -> StructureValue ('StructSum t)
  ProductValue
    :: !(ProductTreeValue t)
    -> StructureValue ('StructProduct t)

data SumTreeValue :: SumTree -> * where
  Sum1Value
    :: (KnownSymbol n)
    => Proxy n
    -> !(StructureValue s)
    -> SumTreeValue ('Sum1 n s)
  Sum2Left
    :: !(SumTreeValue t1)
    -> SumTreeValue ('Sum2 t1 t2)
  Sum2Right
    :: !(SumTreeValue t2)
    -> SumTreeValue ('Sum2 t1 t2)

data ProductTreeValue :: ProductTree -> * where
  Product0Value :: ProductTreeValue 'Product0
  Product1Value
    :: (KnownSymbol n)
    => Proxy n
    -> !(StructureValue s)
    -> ProductTreeValue ('Product1 n s)
  Product2Value
    :: !(ProductTreeValue t1)
    -> !(ProductTreeValue t2)
    -> ProductTreeValue ('Product2 t1 t2)
```

</span>


---

### class Structural

<span style="font-size: 20px">

```haskell
class Structural s where
  type StructKind s :: Structure
  type StructKind s = GStructKind (Rep s)
  toStructValue :: s -> StructureValue (StructKind s)
  default toStructValue
    :: (Generic s, GStructural (Rep s), StructKind s ~ GStructKind (Rep s))
    => s
    -> StructureValue (StructKind s)
  toStructValue s = gToStructValue (from s)
  fromStructValue :: StructureValue (StructKind s) -> s
  default fromStructValue
    :: (Generic s, GStructural (Rep s), StructKind s ~ GStructKind (Rep s))
    => StructureValue (StructKind s)
    -> s
  fromStructValue s = to (gFromStructValue s)
```

* Каждому типу `s` соответстует тип `StructKind s` (да, названия)
* Если `StructKind s1 ~ StructKind s2` значит соблюдается
  *  `(fromJSON :: Value -> StructureValue s2) . (toJSON :: StructureValue s1 -> Value) ~ id`
  * `structureRep :: StructureRep (StructKind s) == structureRep :: StructureRep (StructKind s2)`
* Типы данных, которые мы сохраняем в `jsonb` полях имеют инстансы `Structural`
* Автоматически выводится через `Generic` почти для всех ADT

</span>

---

![bg](why.jpg)

### Зачем это все?

<span style="font-size: 23px;">

```haskell
data Rec = Rec
  { a :: Int
  , b :: Text
  } deriving (Eq, Ord, Show, Generic)

instance Structural Rec

> :t Proxy :: Proxy (StructKind Rec)
Proxy :: Proxy (StructKind Rec)
  :: Proxy
       ('StructProduct
          ('Product2
             ('Product1 "a" 'StructNumber) ('Product1 "b" 'StructString)))

> BL.putStrLn $ encode (structureRep :: StructureRep (StructKind Rec))
{"type":"product","tags":{"a":{"type":"number"},"b":{"type":"string"}}}

> BL.putStrLn $ encode $ toStructValue $ Rec 10 "Hello"
{"a":10,"b":"Hello"}
```

</span>

* Можем получить материализованное представление структуры документа
* Можем сериализовать документ
* Сериализованный докумет всегда соответствует описанию его структуры

---

### Суммы

```haskell
data Sum = S1 Text | S2 Int
  deriving (Eq, Ord, Show, Generic)

instance Structural Sum

> :t Proxy :: Proxy (StructKind Sum)
Proxy :: Proxy (StructKind Sum)
  :: Proxy
       ('StructSum
          ('Sum2 ('Sum1 "S1" 'StructString) ('Sum1 "S2" 'StructNumber)))
> BL.putStrLn $ encode $ (structureRep :: (StructureRep (StructKind Sum)))
{"type":"sum","tags":{"S1":{"type":"string"},"S2":{"type":"number"}}}

> BL.putStrLn $ encode $ toStructValue $ S1 "Hello"
{"tag":"S1","value":"Hello"}

> BL.putStrLn $ encode $ toStructValue $ S2 42
{"tag":"S2","value":42}
```

---

### Более сложный пример

<span style="font-size: 18px;">

```haskell
module Pet.V0 

data Pet
  = Dog
   { name :: Text
   , age  :: Int }
  | Croc
    { name       :: Text
    , teeth      :: Bool
    , tailLength :: Scientific }
  deriving (Eq, Ord, Show, Generic)

instance Structural Pet

module Pet.V1 

data Pet
  = Dog DogRec
  | Croc CrocRec
  deriving (Eq, Ord, Show, Generic)

instance Structural Pet

data DogRec = DogRec
  { name :: Text
  , age  :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Structural DogRec

data CrocRec = CrocRec
  { name       :: Text
  , teeth      :: Bool
  , tailLength :: Scientific }
  deriving (Eq, Ord, Show, Generic)

instance Structural CrocRec
```
</span>

---

![bg](hopa.jpg)

### Хопача. Выкупай

```haskell
> BL.putStrLn $ encode $ toStructValue $ V0.Dog "Spot" 4
{"tag":"Dog","value":{"age":4,"name":"Spot"}}
> BL.putStrLn $ encode $ toStructValue $ V1.Dog $ DogRec "Spot" 4
{"tag":"Dog","value":{"age":4,"name":"Spot"}}
> fromStructValue <$> ((decode $ encode $ toStructValue $ V0.Dog "Spot" 4)) 
    :: Maybe V1.Pet
Just (Dog (DogRec {name = "Spot", age = 4}))
> fromStructValue <$> ((decode $ encode $ toStructValue $ V0.Croc "Chewie" True 2))
    :: Maybe V1.Pet
Just (Croc (CrocRec {name = "Chewie", teeth = True, tailLength = 2.0}))
```
Это потому что

```haskell
> encode (structureRep :: StructureRep  (StructKind V0.Pet)) 
   == encode (structureRep :: StructureRep (StructKind V1.Pet))
True
```

---

### Как можно менять типы?

* Вынос рекорда в отдельный ADT как в примере выше
* Смена порядка полей в структурах
* Смена порядка конструкторов в суммах

---

## А что если хочется изменить типы несовместимым образом?

### На самом деле есть еще одна таблица

```sql
CREATE TYPE doctypeName AS ENUM
  ( 'document', 'action' ) ;

CREATE TABLE versions (
  id                 uuid PRIMARY KEY DEFAULT uuid_generate_v4(),
  created_at         timestamp with time zone DEFAULT NOW() NOT NULL,
  doctype            doctypeName NOT NULL,
  -- Whether it is a "document" or "action" value version
  "version"          bigint NOT NULL,
  structure_rep      jsonb NOT NULL,
  -- Description of the document structure
  UNIQUE (doctype, "version", structure_rep)
);
```

* Поле `structure_rep` содержит `toJSON (rep :: StructureRep s)`
* Если мы случайно изменим хаскельный код так, что структура документа окажется не совместимой с той, что сохранена в базе, то получим ошибку при старте приложения


---

### Что за version и как работает

* Каждый документ имеет номер версии структуры
* Сама структура версии документа сохранена в таблице `versions`
* При старте приложения проверяем таблицу `versions` что структуры документов совместимы с нашим кодом
  * Если в коде появились более свежии версии добавляем их в таблицу `versions`
* Когда загружаем документ проверяем какой он версии
  * Если версия актуальная, то все ок, парсим `Value` как `StructureValue s`
  * Если версия старая, то парсим `Value` как `StructureValue oldS` и потом применяем набор миграций
* Сохраняем докумет всегда в самой последней версии

---

### А откуда мы берем список структур, которые поддерживает наш код?

<span style="font-size: 22px">

```haskell
data Migrations :: N -> [*] -> * where
  Migrate
    :: ( Structural a, Structural b
       , Typeable b, KnownPeano (S n)
       , FromJSON (StructureValue (StructKind b))
       , KnownStructure (StructKind b)
       )
    => Proxy (S n)
    -> (a -> b)
    -> Migrations n (a ': rest)
    -> Migrations (S n) (b ': a ': rest)
  FirstVersion
    :: ( Structural a, Typeable a, KnownPeano n
       , FromJSON (StructureValue (StructKind a))
       , KnownStructure (StructKind a)
       )
    => Proxy n
    -> Proxy a
    -> Migrations n '[a]

type family Head (els :: [*]) where
  Head (a ': rest) = a
```

</span>

---

### Ну очевидно же

<span style="font-size: 22px">

```haskell
-- | Class of obvious transformations of the structure values between each other
class Obvious (s1 :: Structure) (s2 :: Structure) where
  obvious :: StructureValue s1 -> StructureValue s2

obviousMigration
  :: (Structural a, Structural b, Obvious (StructKind a) (StructKind b))
  => a
  -> b
obviousMigration = fromStructValue . obvious . toStructValue
```

* `a -> Maybe a` для любого поля произведения или суммы
* `a -> Vector a` аналогично
* Добавление элемента суммы `A | B -> A | B | C`
* Удаление поля стурктуры `{a: Integer, b: Text, c: Outdated} -> {a: Integer, b: Text}`
* Добавление необязательного поля структуры `{a: Integer, b: Text} -> {a: Integer, b: Text, c: Maybe Text}`
* Обертка в произведение `Text -> {anyName: Text}`
* Обертка в сумму `Text -> AnyName Text`
* Работает рекурсивно

</span>

---

# Как вообще пользоваться

Определим сначала данные которые с которыми работаем

```haskell
data User = User
  { name   :: Maybe Name
  , email  :: Email
  , status :: UserStatus
  } deriving (Eq, Ord, Show, Generic)

instance Structural User

data UserStatus
  = Registered
  | Confirmed
  | Banned
  deriving (Eq, Ord, Show, Generic)

instance Structural UserStatus

data UserAction
  = Init
  | SetName Name
  | SetEmail Email
  | Confirm
  | Ban
  deriving (Eq, Ord, Show, Generic)

instance Structural UserAction
```

---

## Определим функцию трансформации

```haskell
userAction :: PureDocAction User UserAction
userAction = pureDocAction $ \user -> \case
  Init         -> return user
  SetName name -> do
    checkStatus user
    return $ user & field @"name" .~ Just name
  SetEmail e -> do
    checkStatus user
    return $ user & field @"email" .~ e
  Confirm -> do
    checkStatus user
    return $ user & field @"status" .~ Confirmed
  Ban -> return $ user & field @"status" .~ Banned
  where
    checkStatus user = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()
```

## Определим миграции

```
actionMigrations :: Migrations Z '[ UserAction ]
actionMigrations = FirstVersion Proxy Proxy

userMigrations :: Migrations Z '[ User ]
userMigrations = FirstVersion Proxy Proxy
```

--- 

## Init

```haskell
tolstoyAutoInit
  :: forall m n doc act a n1 n2 docs acts
  .  ( MonadPostgres m
     , MonadPostgres n, MonadThrow n
     , StructuralJSON doc, StructuralJSON act
     , HasCallStack
     , Typeable doc, Typeable act
     , doc ~ Head docs
     , act ~ Head acts
     )
  => Migrations n1 docs
  -> Migrations n2 acts
  -> DocAction doc act a
  -> TolstoyTables
  -> n (Tolstoy m doc act a)
```

```haskell
let
    tables = TolstoyTables
      { documentsTable = "documents"
      , actionsTable = "actions"
      , versionsTable = "versions"
      , doctypeTypeName = "doctype"
      }

tolstoy <- tolstoyAutoInit userMigrations actionMigrations userAction tables
```

---

## Tolstoy

```haskell
data Tolstoy m doc act a = Tolstoy
  { newDoc
    :: doc
    -- ^ Initial state of the doc.
    -> act
    -- ^ Initial action. It will not be performed on given doc, only
    -- written to DB
    -> m (TolstoyResult (DocDesc doc act))
  -- ^ Inserts a new document in DB
  , getDoc
    :: DocId doc
    -> m (Maybe (TolstoyResult (DocDesc doc act)))
  -- ^ Get last version of some object
  , getDocHistory
    :: DocId doc
    -> m (Maybe (TolstoyResult (DocHistory doc act)))
  -- ^ Get full history of the document
  , changeDoc
    :: DocDesc doc act
    -> act
    -> m (TolstoyResult ((DocDesc doc act), a))
  -- ^ Saves changed doc to the DB. Note that it does not check the
  -- document history consistency from the business logic perspective
  , listDocuments :: m (TolstoyResult [DocDesc doc act])
  } deriving (Generic)
```

---

## DocDesc

```haaskell
data DocDesc doc act = DocDesc
  { document        :: !doc
  , documentId      :: !(DocId doc)
  , documentVersion :: !Integer
  -- ^ Original version number before migration
  , action          :: !act
  , actionId        :: !(ActId act)
  , actionVersion   :: !Integer
  -- ^ Original version number before migration
  , created         :: !UTCTime
  , modified        :: !UTCTime
  } deriving (Eq, Ord, Show, Generic)
```

---

## DocHistory

```haskell
data DocHistory doc act = DocHistory
  { documentId :: !(DocId doc)
  , created    :: !UTCTime
  , history    :: !(NonEmpty (Story doc act))
  -- ^ Story points in reverse order. Head is the last actual version
  -- and tail is the initial
  } deriving (Eq, Ord, Show, Generic)
  
data Story doc act = Story
  { document        :: !doc
  , documentVersion :: !Integer
  -- ^ Original document version number before migration
  , action          :: !act
  , actionId        :: !(ActId act)
  , actionVersion   :: !Integer
  -- ^ Original action version number before migration
  , modified        :: !UTCTime
  , parentId        :: !(Maybe (ActId act))
  } deriving (Eq, Ord, Show, Generic)
 
```

---

## Сохраним документ текущей версии в базу

```haskell
Right doc <- runTest p $ newDoc t 
  (UV0.User Nothing (Email "a@b.com") UV0.Registered) UV0.Init 

[Debug]  INSERT INTO "actions" (document, document_version, action, action_version) VALUES ( '{"email":"a@b.com","status":{"tag":"Registered","value":{}},"name":null}', 0 , '{"tag":"Init","value":{}}', 0 ) RETURNING id, created_at @(main:Tolstoy.DB.Init /home/razor/work/tolstoy/src/Tolstoy/DB/Init.hs:228:57)
[Debug]  INSERT INTO "documents" (action_id) VALUES ( '361e1d49-0e0f-45bd-8505-59a31df21b6d' ) RETURNING id, created_at @(main:Tolstoy.DB.Init /home/razor/work/tolstoy/src/Tolstoy/DB/Init.hs:235:58)
```

---

## Допустим захотели поменять структуру документа

```haskell
data User = User
  { name   :: Maybe Name
  , email  :: Email
  , status :: UserStatus
  , phone  :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance Structural User
```

* Зафейлится при `tolstoyAutoInit`
** Exception: DatabaseHasIncompatibleMigration ....

---

## Значит надо сделать миграции

```haskell
data User = User
  { name   :: Maybe Name
  , email  :: Email
  , status :: UserStatus
  , phone  :: Maybe Text
  } deriving (Eq, Ord, Show, Generic)

instance Structural User

data UserStatus
  = Registered
  | Confirmed
  | Banned
  | Blessed
  deriving (Eq, Ord, Show, Generic)

instance Structural UserStatus

data UserAction
  = Init
  | SetName Name
  | SetEmail Email
  | Confirm
  | Ban
  | Bless
  | SetPhone Text
  deriving (Eq, Ord, Show, Generic)

instance Structural UserAction
```

---

```haskell
userAction :: PureDocAction User UserAction
userAction = pureDocAction $ \user -> \case
  Init         -> return user
  SetName name -> do
    checkStatus user
    return $ user & field @"name" .~ Just name
  SetEmail e -> do
    checkStatus user
    return $ user & field @"email" .~ e
  Confirm -> do
    checkStatus user
    return $ user & field @"status" .~ Confirmed
  Ban -> return $ user & field @"status" .~ Banned
  Bless -> do
    checkStatus user
    return $ user & field @"status" .~ Blessed
  SetPhone p -> do
    unless (T.all C.isDigit p) $ do
      Left "Phone is invalid"
    return $ user & field @"phone" .~ Just p
  where
    checkStatus user = case status user of
      Banned -> Left "User is banned"
      _      -> pure ()
```

---

## Миграции

```haskell
actionMigrations :: Migrations (S Z) '[ UserAction, V0.UserAction ]
actionMigrations
  = Migrate Proxy obviousMigration
  $ V0.actionMigrations

userMigrations :: Migrations (S Z) '[ User, V0.User]
userMigrations
  = Migrate Proxy obviousMigration
  $ V0.userMigrations
```
---

## При старте обновится таблица versions 

```
t1 :: Tolstoy TestMonad _ _ _ <- runTest p $ tolstoyAutoInit 
  UV1.userMigrations UV1.actionMigrations UV1.userAction tables
```

```sql
INSERT INTO
  "versions" (doctype, "version", structure_rep)
VALUES
  (
    'document',
    1,
    '{"type":"product","tags":{"email":{"type":"string"},"status":{"type":"sum","tags":{"Banned":{"type":"product","tags":{}},"Blessed":{"type":"product","tags":{}},"Registered":{"type":"product","tags":{}},"Confirmed":{"type":"product","tags":{}}}},"phone":{"argument":{"type":"string"},"type":"optional"},"name":{"argument":{"type":"string"},"type":"optional"}}}'
  ),
  (
    'action',
    1,
    '{"type":"sum","tags":{"Confirm":{"type":"product","tags":{}},"Init":{"type":"product","tags":{}},"Ban":{"type":"product","tags":{}},"SetName":{"type":"string"},"Bless":{"type":"product","tags":{}},"SetEmail":{"type":"string"},"SetPhone":{"type":"string"}}}'
  )
```  
---

![bg](wow.jpg)
## Теперь мы можем прочесть старый документ

```haskell
Just (Right doc2) <- runTest p $ getDoc t1 
  (coerce $ doc ^. field @"documentId") 
```

```haskell
> doc2 ^. field @"document" 

User { name = Nothing
     , email = Email {unEmail = "a@b.com"}
     , status = Registered
     , phone = Nothing 
     }
```

---

# Статус 

* Работают миграции и тесты
* Интерфейс пока страшный, но можно лучше
* Нужен рефакторинг

---

# Что дальше

* `StructureQuery` чтобы 
  ```haskell
  data StructureQuery :: Structure -> * where
    StringExactly :: Text -> StructureQuery 'StructString
    StringLike :: Text -> StructureQuery 'StructString
  ...
  ```
* Выделить `Structure` в отдельный пакет или типа того
* Бенчмарки и еще больше тестов
* Отвязать бэкэнды

---


![center 176%](mkay.jpg)
