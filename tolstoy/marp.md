# Какие проблему решаем

* Очень сложный и тупой код с крудами
  * Да, это частично решается с помощью `pg-schema`
* Остается проблема истории
* Проблема размазанности логики изменения документов по коду
* Документы вообще не надо размазывать по таблицам

---

# Как

* Храним документ `jsonb` полем
* Документы изменяются чистой функцией `docAction :: Document -> ActionData -> Document`
* Рядом с документом храним данные для функции `docAction` тоже в виде `jsonb`
* Используем автоматические миграции с защитой от случайного изменения кода

---

<style>
  .slide {
    font-size: 20px !important;
  }
</style>

## Схема

```sql
CREATE TABLE actions (
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
  action_id          uuid NOT NULL REFERENCES ^{actions}(id)
  -- The reference to the latest action of the document. The head of
  -- the liked list formed by the "parent_id" reference in the
  -- "actions" table.
);
```

---

## Как работают миграции

Имеем закрытый кайнд `Structure`

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
* Все суммы и проиведения тегированы

---

### StructureRep

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

* Описывает **структуру** документа (не его содержание)
* Имеет инстансы `FromJSON`/`ToJSON`

---

### StructureValule

<span style="font-size: 75%">

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

* Описывает **значение** документа
* Имеет инстансы `FromJSON`/`ToJSON`

---

### class Structural

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
* Если типы `StructKind s1 ~ StructKind s1` значит у нас есть `(fromJSON :: Value -> StructureValue s2) . (toJSON :: StructureValue s1 -> Value) ~ id`
* Типы данных, которые мы сохраняем в `jsonb` полях имеют инстансы `Structural`
* Автоматически выводится через `Generic` почти для всех ADT

---

### class KnownStructure

```haskell
-- | Materialize any structure type to it's representation
class KnownStructure (s :: Structure) where
  structureRep :: StructureRep s
```

* Для всех `s`, если у нас есть `StructKind s` то для него можем получить значение `StructureRep s`
* Значение `StructureRep s` тоже сохраняется в базу!

---

### Попробуем использовать

```
> data A = A { name :: Text } deriving Generic
> instance Structural A
> BL.putStrLn $ encode (structureRep :: StructureRep (StructKind A))

{"type":"product","tags":{"name":{"type":"string"}}}

> BL.putStrLn $ encode $ toStructValue $ A "Hello"

{"name":"Hello"}
```

---

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

<span style="font-size: 80%">

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

* Два списка миграций, один для документов, второй для действий
* Можем получить список `[(Integer, Value)]`
  * Это и есть наш список версий документа, мы его сравниваем с содержимым таблицы `versions`
* Для любого `s` из списка типов `list` в `Migrations N list` можем получить функцию `migrate :: s -> Head list`
* Функции миграции над простыми ADT
* Функции можно ручные, а можно автоматические

---

### Ну очевидно же

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

* `a -> Maybe a` для любого поля продакта или суммы
* `a -> Vector a` аналогично
* Добавление элемента суммы `A | B -> A | B | C`
* Удаление поля стурктуры `{a: Integer, b: Text, c: Outdated} -> {a: Integer, b: Text}`
* Добавление необязательного поля структуры `{a: Integer, b: Text} -> {a: Integer, b: Text, c: Maybe Text}`
* Обертка в произведение `Text -> {anyName: Text}`
* Обертка в сумму `Text -> AnyName Text`
* Работает рекурсивно

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

## Допустим захотели поменять структуру документа

```haskell



---

# Что дальше

* Отвязать бэкэнды
* `StructureQuery`

---

# Вопросы

![auto 150%](mkay.jpg)
