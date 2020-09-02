# COVID_LA_Plots
Code for Shiny app to explore LA COVID data: https://victimofmaths.shinyapps.io/COVID_LA_Plots/
<br><br>
Much credit is due [@lukejostins](https://twitter.com/lukejostins) to who took a previous version of my messy code and turned it into [the first version of this app](https://victimofshiny.shinyapps.io/shiny/).<br><br>
[server.R](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/server.R) and [ui.R](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/ui.R) contain the actual code for the app, while [UnderlyingCode.R](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/UnderlyingCode.R) is the code that generates the various .csv files.
<br><br>
Suggested citation for the app: Angus, Colin (2020): COVID-19 Local Authority Death and Case Plots. The University of Sheffield. Online resource. https://doi.org/10.15131/shef.data.12658088
<br><br>
The app produces 8 different sets of plots for England, Northern Ireland Scotland, Wales and every Lower Tier Local Authority/Council Area although not all plots are available for all countries.<br><br>
I've also compiled a set of automatically-updating tables based on the same data, which show the local areas with the highest numbers and rates of COVID-19 cases in the past week and the biggest increases in case numbers/rates. You can find these at (https://victimofmaths.github.io/COVID_LA_Plots/). Many thanks to [@Fcorowe](https://twitter.com/Fcorowe) for helping me understand how to set this up.
<br><br>
![Total excess deaths](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_1.png)
![Excess deaths by cause](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_2.png)
![Excess deaths by location](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_3.png)
![Cases vs deaths](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_4.png)
![Case numbers](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_5.png)
![Cases by pillar](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_6.png)
![Comparative case rates](https://github.com/VictimOfMaths/COVID_LA_Plots/blob/master/COVID_LA_Plots_7.png)
