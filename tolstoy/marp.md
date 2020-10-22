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

Описывает **структуру** документа (не его содержание)

--- 

### StructureValule

<span style="font-size: 80%">
  
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
* Имеет инстансы `FromJSON/ToJSON` для любых `s :: Structure`

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

* Типы данных, которые мы сохраняем в `jsonb` полях имеют инстансы `Structural`
* Автоматически выводится через `Generic` почти для всех ADT
* 



--- 

# Вопросы

![auto](mkay.jpg)
