defmodule Schemata.BaseMigration do
  use Schemata.Migration, [
    authored_at: "2016-08-18T18:07:04Z",
    description: "Base schema"
  ]

  def up do
    :ok
  end
end
