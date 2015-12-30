# grtrends 1.3.2 (unreleased)

- Now able to specify up to five countries (#53) via `gtrends("NHL", geo = c("CA", "US"))`
- Fixing issue #51 allowing UK-based queries via `geo="GB"`

# gtrendsR 1.3.1

- Fixing issue #34 where connection verification was not done properly.
- Now able to use more latin character in query. For example: `gtrends("montréal")`.
- Can now deal with data returned other than in English language.

# gtrendsR 1.3.0

- First version of gtrendsR
