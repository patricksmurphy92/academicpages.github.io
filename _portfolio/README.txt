# GT Football Tickets Willingness-to-Pay by Section

## DESCRIPTION:
This repo contains all the data and code necessary to visualize Willingness-to-Pay for a ticket in each section of Bobby Dodd Stadium.

Select an opponent from the 2018 football season schedule and a potential day and time combination to see the predicted net difference between Willingness-to-Pay on the secondary market and the face value for which the ticket was originally issued. A heatmap-style, gradient color scale shows the sections that often sell below face value (colored red), above face value (colored green), and immediately around face value (colored light yellow). Hovering above a section also reveals a tooltip showing the section name, Willingness-to-Pay, 90% confidence interval (bounds), face value, and net difference (face value - Willingness-to-Pay).

## INSTALLATION:
To use this repo, clone to your local drive.

	$ git clone https://github.gatech.edu/rwiseley3/CSE6242.git stadium_vis

## EXECUTION:
Navigate into the newly created folder containing all the repository materials and start a local server. For example, if you have a MacBook with Python 3 as the default you can run the following command in the terminal:

	$ cd stadium_vis
	$ python -m http.server

Then open up your browser (we recommend Chrome) at the localhost port specified by your computer (e.g. http://localhost:8000), and navigate to /bobbydodd.html.
Once there you should be able to see and interact with the visualization. Don't forget to kill you local server after you are finish (ctrl-c for Macs).

## Authors
* Kyle Jurczak
* Patrick Murphy
* Brian Price
* Addison Rogers
* Will Sweet
* Rachel Wiseley