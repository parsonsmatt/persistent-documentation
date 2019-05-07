# `User`

you can use string literals to write documentation for the entity itself. The strings will be mappended together, so you'll need to handle whitespace yourself.

* Primary ID: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) | You can document the user's ID field. |
| `firstName` | string | The user's first name. |
| `active` | boolean | Whether or not the user is able to log in. |

# `Dog`

* Primary ID: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | string |  |
| `toy` | string |  |

# `UserDog`

Users can have many dogs, and dogs can have many users.

* Primary ID: `id`

| Column name | Type | Description |
|-|-|-|
| `id` | integer (64) |  |
| `dog` | string | This should have type text. |
| `user` | integer (64) |  |

