# gtrendsR 1.3.3

- A ggplot2 object can now be returned for further customization. `plot(gtrends("NHL")) + ggtitle("NHL trend") + theme(legend.position="none")`

- Support for hourly and daily data (#67). For example, it is now possible to have hourly data for the last seven days with `gtrends("nhl", geo = "CA", res = "7d")`. Use `?gtrends` for more information about the time resolution supported by the package.

- Support for categorties (#46). Ex.: `gtrends("NHL", geo = "US", cat = "0-20")` will search only in the *sport* category.

- Some countries (ex: Hong Kong) were missing from the list (#69).

- Various typos and documentation work.

# gtrendsR 1.3.2

- Added support for sub-countries (#25). Ex.: `gtrends("NHL", geo = "CA-QC")` will return trends data for Québec province in Canada. The list of supported sub-countries can be obtained via `data(countries)`.

- Data parsing should work for any data returned by Google Trends (i.e. countries independent).

- Better support for queries using keywords in different languages (#50, #57). Ex.: `gtrends("蘋果", geo = "TW")`

- Now able to specify up to five countries (#53) via `gtrends("NHL", geo = c("CA", "US"))`

- Fixing issue #51 allowing UK-based queries via `geo = "GB"`

# gtrendsR 1.3.1

- Fixing issue #34 where connection verification was not done properly.
- Now able to use more latin character in query. For example: `gtrends("montréal")`.
- Can now deal with data returned other than in English language.

# gtrendsR 1.3.0

- First version of gtrendsR
