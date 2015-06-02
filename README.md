# cricketr

When limited-overs cricket games are stopped prematurely for weather-related or other reasons, 
*stopping rules* such as the [Duckworth-Lewis method](http://en.wikipedia.org/wiki/Duckworth%E2%80%93Lewis_method)
are used to determine which team won, i.e. how many runs the team that batted second should have scored
(its *target*), considering the number of overs played and wickets lost, in order to be considered the winner.

While the Duckworth-Lewis method is "an attempt to set a statistically fair target for the second team's innings",
its actual statistical properties have rarely been evaluated objectively and independently. This repository includes/will include:

* code for scraping relevant match data from ESPN's web site 
(as far as we can tell, such scraping is within the terms of service provided the
results are not redistributed)
* code for tabulating resource tables (% of resources remaining as a function of
number of overs played and wickets lost)
* different methods of estimation of resource surfaces (essentially stoppage-rule tables)
from raw resource tables
* tools for evaluating methods (RMSE, kappa, bias, etc.) and applying cross-validation
