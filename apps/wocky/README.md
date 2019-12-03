This application defines the business logic and database persistence for the
Wocky service. If Wocky is the backend for the TinyRobot mobile app, then this
application is the backend of the backend.

The code in this application should follow the conventions defined and enforced
by Credo. However, there are also a few conventions specifically related to
Ecto.

=== Organization
==== Contexts
Aside from a few utility modules at the top level, the code in this application
is organized into "contexts." These contexts are meant to serve the same role
as the bounded contexts in an idiomatic Phoenix application. Each context is
organized into a directory that contains three types of modules:

1. A context module that provides the public API for the context
2. Ecto schema modules
3. Utility modules

Context modules `use Wocky.Context` and should have an appropriate name.

When a module outside of the context needs to call code within the context it
should always (with very few exceptions) be calling a function in the context
module. It isn't necessary that all of the logic be defined within the context
module; the context module may delegate some of the logic to utility modules and
then expose the appropriate function via `defdelegate`.

This advice also applies to types. The goal is to have modules outside of the
context referencing internal context modules as little as possible, if at all.

===== Context Naming
Since naming is one of the two hard problems in computer science, there are
only very general guidelines available to help in choosing appropriate context
names.

There are generally three types of contexts: data management, process
abstractions, and collections of similar types of modules. Data management
contexts are primarily concerned with the persistence of entities in the
database and the business logic that goes along with them. This type of context
will contain primarily Ecto schemas. Process abastraction contexts may also
contain Ecto schemas, but their primary purpose will be to encapsulate business
logic and algorithms. The final type of context is primarily used to organize
several similar modules, such as callback handlers or event definitions. Any
given context is likely to be some mixture of the three, but will usually be
more representative of one that the others.

An appropriate name for the context depends on which type it is.  When the
context is primarily concerned with managing data of one type, or from one
table, then the context should be named after that type. For example, the
context which manages database entries of type `Block` should be named
`Blocks`. When it is managing several types of related data, then the name
should reflect the archetype of the data it is representing.

Contexts that are primarily process abstractions often are focused around the
processing, but not management, of a particular type of data or event. In that
case, the name should reflect the core data or event that is being processed.
These context names are usually pluralized to distinguish them from the actual
data or event being processed. A more general process which is not concerned
with any one type of entity, such as authentication, would probably not have a
pluralized name.

The final type of contexts are probably the easiest to name. The name should be
pluralized and represent the types of modules; i.e., `Callbacks` and `Events`.

Naming is important since it has a big impact on the readability and
maintainability of the code in general. It is also a good way to gauge the
quality of the design of the context itself. A context that is very hard to name
may poorly designed. Consider breaking the context into multiple, smaller ones,
or, if the context is already quite small, including the code in another
context.

==== Schemas
Schema modules `use Wocky.Repo.Schema`. They should only define three things:

1. An Ecto schema (required)
2. One or more changeset functions (usually required)
3. Functions that return composable queries (optional)

In other words, public functions defined in a schema module should only ever
return a value of type `Changeset.t()` or `Queryable.t()`.

Utility modules should never be called from outside the context. If some
function in a utility module is needed outside of the context, then the
function can be exported by the context module via delegation.

=== Ecto Conventions
These are some general guidelines for how we prefer to write Ecto code. They are
guidelines, and not hard and fast rules, and they may be ignored when there is
a *very good* reason to do so.

==== Query Syntax
Prefer Ecto's "keyword syntax" over the "expressions syntax." For example,
prefer this:

```elixir
from o in Object,
  where: o.field == ^something,
  select: o.thing
```

over this:

```elixir
Object
|> where(field: ^something)
|> select([o], o.thing)
```

This is a weak preference, and there are some cases where it should be ignored.
For example, when chaining several composable queries together, a pipe chain
may make more sense than trying to fit things into the keyword syntax. In that
case, it is preferable to define each query fragment using the keyword syntax
so that the pipe chain will only include local function calls and no calls into
`Ecto.Query`.

==== Associations
Prefer using associations to passing foreign keys into queries. For example,
prefer this:

```elixir
assoc(user, :things)
```

over this:

```elixir
from t in Thing,
  where: t.user_id == ^user.id
```

The same applies when building objects to be saved to the database. Use
`build_assoc` rather than pass a foreign key as a param.

```elixir
user
|> build_assoc(:things)
|> Thing.changeset(%{data: data})
|> Repo.insert()
```

is preferable to this:

```elixir
%{user_id: user.id, data: data}
|> Thing.changeset()
|> Repo.insert()
```

Finally, don't define an association in an Ecto schema unless it is going to be
used. That having been said, it should be rare indeed for a table to have a
foreign key and the association is never used.
