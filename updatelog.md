# A Topology-Based Framework for Assessing the Statistical Robustness of Multi-Layer Coastal Infrastructures - Update log from beginning to end.

## 6/23 - began reading on certain papers, Betti numbers, the use of abnormal cases and attacks on nodes using tology based modeling.

Im pretty new to these topics, i should definitely do some looking into what betti numbers are and their roles in whatever im reading as they appear somewhat frequently. I have began to look into the new program that i will be writing in, which will be R.

R seems like an interesting language, some aspects of C and python maybe, ill need to get some more training on this.

I was able to find a few papers based on the topic of resilience based on extreme events and such, i havent gone to in depth into each one, but reading the abstracts look promising.

## 6/24 - Tested out some R code, Tried using some multinet, but the tool was causing many errors.

Bit of a stressful day, lots of code wasnt running well, 

Im currently trying out QGIS, Multinet, and some mapping tools to try and get the database of power plants to appear on that map.

Program is causing some issues, despite restarting, so ill probably continue to read though some papers until the end of the day.

## 6/25 - Translating to python and trying again

Based on my progress from yesterday, i decided to translate some of the graphing code and 3D functions to Python and crated some simple programs which worked, so thats neat, i also was able to get a hold of some sourecs to create networks, I tried out R's visnetwork and i graph, and i was able to create a neat little graph with nodes pointing to each other, kind of tapping into that part where we need to see where sources are dependent on certain networks.

With some transalting, i got the hang of it a little bit, i actually got a mini network to appear using R's visNetwork, Awesome!!

Although its just a simulation with some practice data, i think this is a pretty big move.
![{A03353E2-C645-45E7-A09D-1A309E95E034}](https://github.com/user-attachments/assets/12a0efdc-cffd-44fe-a655-c9b0b7a6b443)

I have begun to try out some other tools, ggplot2, sf, rnaturalearth, osmdata, dplyr, mapview.

## 6/26 - Reading on more documents, waiting on faculty mentor to post some R programs to view

Ill continue on trying out some network tools, Im thinking of trying to place these network nodes on a real map to show the cascading effects eventually, at the moment, i am trying out OpenStreetMap and QGIS to see what i can do with these tools. Ill try to create a simulation for California first, but then i want to try to do an almost global graph..


To my surprise, we got something working amazingly, this was able to be generated using an osm file, but the nodes are expanding thoughout the coast of the state, the problem might be the power lines, as theyre highlighting every road, even freeways, which im not sure should be correct, at the moment they're only covering the area in which i downloaded the .osm file for, im also guessing its locating underground power lines, which is otherwise fine.

![{A2E1F191-97DA-4F9F-BECD-8CA345D5C5B0}](https://github.com/user-attachments/assets/ce8d0fda-f063-4316-8b76-ccc98ce2dcb3)


## 6/27 - Writing Presentation on key paper

im diving farther into the reasearch paper my faculty mentor has provided me, i plan to present this on Tuesday, July 1st, so far im starting to understand the concepts, but i need to look into the mathy stuff more.

I finished up the day with reading and really diving into the paper that Dr. Dorcas provided me, there are alot of cool therories that i have been reading on based on the paper, like the vietoris rips complex and simplices leading into Betti numbers. Next week i will finish up my presentation for the key paper i found.


## 6/30 - Finishing presentation on key paper

Today im working on finishing the presentation as mentioned beforehand, once i present and finish up with that, ill begin to look at papers that actually dive into the aspects of extreme weather or disasters. If possible, i hope Dr. Dorcas can provide me the code from R so i can have a better, general idea of what to program..

## 7/1 - Reading through key paper presentation

## 7/2 - Found a SUPER key paper, this has a lot of similarties of what i am planning on, and i can create something possibly more advanced..

## 7/3 

## 7/4 

## 7/5

## 7/6

## 7/7 - Met with Dr. Ofori-Boateng - Lots of info!

There was alot of information to take in and learn about, there is this software called PowerWorld, that seems to be like a data flow software for grids that i am being provided with.

The current goal is to create some files based on a 10,000 bus grid based on the western United States.

To begin i will be doing some practice code as well as some tests using the 118 and 2000 bus grid provided to me as well, but ill try to ramp up to the 10,000 grid as soon as possibe,

## 7/8 - Texas grid began to work and completed practice code

The code was all straightforward, i finished the data pretty early on, so now i think i will focus a bit more on some information based on power world and some tools i can use to begin to develop the 10,000 bus power grid.

## 7/9 - PowerWorld dive-in and beginning of power grid development

I spent most of the day learning about PowerWorld and what it can do in power systems, im pretty restricted though, to only do up to 13 bus grids, but its giving me some opportunity to learn about these systems. Pretty interesting! 

I also began to develop the map and grids for the 10k power grid.

## 7/10 - Beginning to develop 10k Power grid - Success!

I began to develop the powergrid and it was a success.

I was also able to develop a simple program that removes nodes, so ill try to make something semi-realistic, our direction to this is going to be to develop a wildfire scenario as i think that is most compatible to some events that take place in the western united states.

## 7/11 - First concept and test of node attacks have been created!

Super exciting update for the project as now it will serve as a foundational step to what i need to create for the databases.

## 7/14 - Met with Dr. Ofori-Boateng, some redirection, but otherwise doing good progress.

Missed some ideas on relationship matrices, but the majority of data i need is already created, so i think this can be taken care of quickly.

I began to make my graphs more dynamic, separating data from buses, branches, and other sectors. Switched to Leaflet for graph generation.

## 7/15 - Got some color to separate buses, generators, and loads. First wildfire simulations!!!

This is good, as we can separate the data as well as observe the types of generators we have to use in our simulation.

Whats awesome is we now have more dynamic wildfire simulations, this will be helpful to siulate some sections, but we need to also find a way to use databases.

## 7/16 - Outage grids, Resilience graphs,

We got some grayed out areas to show where the outages are, as well as graphs displaying the resilience and cascading fall of the data once these nodes are attacked, i think this tool will be super useful.

I began to also work on my progress update that will be presented on Thursday (tomorrow).

## 7/17 - Progress presentation!!
Took some time to create a presentation on my current progress and after to do some reflection, i believe i am on the right track to complete this.

## 7/18 - Built the relationship matrices!!
Pretty awesome results as i got the required relationship matrices from the data, need to normalize the now.

## 7/21 - Met with Dr Ofori-Boateng, Building persistence diagrams for simulation,
Met with my faculty mentor, looks like we are on the right track and about 70% of the way there, thats some sweet news to me!, i need to create some persistence diagrams instead of barcodes though, so ill work on that too.

In terms of running these relationship matrices..

Takes quite a while, but im getting results by testing in smaller sets, so ill try and run a large scale set and see how long it takes.

