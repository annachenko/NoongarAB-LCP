extensions [
 Rnd gis]
globals [
 csr
;A* variables
 p-valids Start Final-Cost
;weighted walk variables
 start-point end-point a-star-path]
patches-own [
;A* variables
 father Cost-path visited? active? best-path?
;weighted walk variables
 access popularity accessed? suitability distance-from-neighbour distance-from-path closest-patch
;variables_normalised
 A DFP DTG]
turtles-own [
;weighted walk variables
 goal-achieved? goal next-patch unsuccessful-target memory alive? ]
breed [traveller travellers]

to setup
  ca
  reset-ticks

;;setup world;;
 ;load data
 set csr gis:load-dataset "data/testsite2.asc"
 ;resize world and set envelope
 resize-world 0 gis:width-of (csr) - 1 0 gis:height-of (csr) - 1
 gis:set-world-envelope (gis:envelope-of csr)
 ;apply access values from CSR into landscape
 gis:apply-raster csr access
 ;set negative values to 0
 ask patches [  ifelse (access <= 0) or (access >= 0)  [ ] [ set access 0]  ]
 ;recolour patches based on access variable
 ask patches [
    ifelse access > 0 [
      set pcolor scale-color green access 1 0  ;set up world to show areas of greatest access
    ][
      set pcolor blue]
  ]

;setup cultural points
 ask patch 92 193 [
    set pcolor orange] ;start
 ask patch 33 57 [
    set pcolor red] ;end
;;A* set up;;
;initial values of patches for A*
  ask patches [
    set father nobody
    set Cost-path 0
    set visited? false
    set active? false
    set best-path? 0
  ]
;set valid patches (land only)
    set p-valids patches with [pcolor != blue]

 ;create start
    set Start one-of patches with [pcolor = orange]

  ;create a turtle to draw the path at the end
    crt 1
    [ht ;hide turtle
     set size 1
     set pen-size 2
     set shape "square"
    ]
end

;;A* reporting;;
;report total cost (G cost + H cost)
to-report Total-expected-cost [#Goal]
  report Cost-path + Heuristic #Goal
end
;report H cost
to-report Heuristic [#Goal]
  report distance #Goal
end
;A* algorithm
to-report A* [#Start #Goal #valid-map]
 ;clear all the information in the agents and reset them
  ask #valid-map with [visited?]
  [
    set father nobody
    set Cost-path 0
    set visited? false
    set active? false
  ]
 ;activate the starting point to begin the searching loop
 ask #Start
  [
    set father self
    set visited? true
    set active? true
  ]
  ;exists indicates if in some instant of the search there are no options to continue
  ;i.e. no path connecting #Start and #Goal
  let exists? true
  while [not [visited?] of #Goal and exists?] ;searching loop is executed when we havent reached the #Goal and we think a path exists
  [let options #valid-map with [active?] ;only work with valid patches that are active
    ifelse any? options

    [
      ask min-one-of options [Total-expected-cost #Goal] ;take active patch with min cost
      [
        let cost-path-father cost-path ;store its real cost / G cost (to reach it) to compute real cost of its children
        set active? false ;deactivate it
        let valid-neighbors neighbors with [member? self #valid-map] ;compute valid neighbors and look for extension of path
        ask valid-neighbors
        [
          let t ifelse-value visited? [total-expected-cost #Goal][2 ^ 20]
          ;t = temporal cost. if visited, calculate the expected cost. If not, create big number. This ensures that the new path will be smaller
          if t > (cost-path-father + distance myself + Heuristic #Goal)
          ;if the old total-expected-cost is greater than the new one, we update the patch information to show the new cost.
          ;the current patch becomes the father of its neighbor in the new path
          [
            set father myself
            set visited? true
            set active? true
            set cost-path cost-path-father + distance father ;store real cost in the neighbour from the real cost of its father
            set Final-Cost precision cost-path 3 ;reports cost-path rounded to 3 decimal places
    ]]]]
            ;else
    [set exists? false
  ]]
  ifelse exists? ;after the searching loop, if there exists a path
  ;we extract the list of patches in the path, form #Start to #goal by jumping back from #Goal to #start by using the fathers of every patch
  [
    let current #Goal
    set Final-Cost (precision [Cost-path] of #Goal 3)
    let rep (list current)
    while [current != #Start]
    [set current [father] of current
      set rep fput current rep
    ]
    report rep
  ]
  [
    ;otherwise there is no path and we return false
    report false
  ]
end
;draw A* path
to Look-for-Goal
  let A-Goal one-of patches with [pcolor = red]
  let path A* Start A-Goal p-valids ;compute path between Start and Goal
  if path != false [
    ask turtle 0 [set color yellow]
    foreach path [
      p -> ;creates and reports an anonymous procedure, p = argument that can be used like let
      ask turtle 0 [
        move-to p
       set best-path? 10
        stamp
        ]
    ]
      ;set goal and the new start point
      set Start A-Goal
    ]
  ;Calculate euclidean distance of surrounding 10 patches from A* path
  set a-star-path patches with [best-path? = 10]
  ask a-star-path [
    ask patches in-radius 10 with [pcolor != blue]
    [
      ifelse best-path? = 0
      [
      set closest-patch min-one-of patches with [best-path? = 10] [distance myself]
      ifelse closest-patch = nobody
      [set distance-from-path  0]
      [set distance-from-path 10 - (distance closest-patch )
          if distance-from-path < 1 [ set distance-from-path 0]]

        ]
      [set distance-from-path 10
  ]
  ]
  ]
end

;;weighted walk set up;;
to set-up-weighted-walk
  ;clear ticks and patches
  reset-ticks
  ask patches [
    set accessed? 0]

;create start and end points based on coordinates
  set start-point patch 92 193
  set end-point patch 33 57

;set up traveller at start point
ask start-point [
    sprout-traveller 1[
      set shape "person"
      set color random 50
      set size 1
      pen-down
      set pen-size 0.01
      set goal-achieved? false
      set goal end-point
      set memory (patch-set)
      set alive? true]
  ]

end

;;run weighted walk;;
 to weighted-walk
;if the agent does not reach its goal on time, remove its path from patch popularity and kill it
  if ticks >= ticks_limit [
    ask traveller [
      ask memory [set popularity (popularity - 1)]
      set goal-achieved? false
      die
    ]
  ]
  ask patches [
    if count turtles-here > 0 and accessed? = 1 [
    set popularity (popularity + 1)]
  ]
;stopping procedure
 if all? traveller [goal-achieved?]
  [stop] ; if everyone has reached their goal, stop
 if all? traveller [access = 0]
  [stop] ;if everyone is stuck, stop
;moving to goal
 ask traveller [
     ifelse distance goal <= 1 [ ;if next to goal, move to goal
     move-to goal
     set accessed? (accessed? + 1)
     set goal-achieved? true
     set memory (patch-set memory patch-here)
     die
     ]

    [;calculate the euclidean distance from the agent to the goal
      let turtle-to-goal distance goal
      ask neighbors [
        set distance-from-neighbour distance [goal] of myself
        ;normalising weights
        set A access
        set DFP distance-from-path / 10
        set DTG ( ((turtle-to-goal - distance-from-neighbour) + 1.41421356) / (1.41421356 * 2))
        ;suitability equation
        set suitability ((access_weight * A) + (dtp_weight * DFP) + (togoal_weight * DTG))
        if suitability < 0 [
          set suitability 0]
    ]
      ;use roulette wheel selection through rnd extension to decide next patch
      set next-patch rnd:weighted-one-of neighbors with [access > 0 and accessed? = 0] [ suitability]
      ifelse next-patch != nobody
        [move-to next-patch
          set accessed? (accessed? + 1)
      set memory (patch-set memory patch-here)]
      ;if roulette wheel selection doesnt work, just pick the patch with the highest suitability value
      [set unsuccessful-target max-one-of neighbors with [(accessed? = 0) and (access > 0)] [suitability]
        ifelse unsuccessful-target != nobody
        [move-to unsuccessful-target
          set accessed? (accessed? + 1)
          set memory (patch-set memory patch-here)]
        [move-to one-of neighbors with [access > 0]
        set accessed? (accessed? + 1)
        set memory (patch-set memory patch-here)]
      ]
  ]
  ]
tick
end

;;show popularity in interface;;
to show-popularity
  clear-drawing
  ask patches [
    if popularity > 1 [
      set pcolor scale-color red popularity 10 1]
  ]
end
;;report the number of agents that reached goal;;
to-report success-rate
  report [popularity] of patches with [pcolor = red]
end

;;report the popularity of gnamma boorna patches (used for sensitivity analysis);;
to-report boorna-gnamma-popularity
report [popularity] of patches with [pcolor = brown]
end

;;export popularity surface as ascii file;;
to store-output
let patch_popularity gis:patch-dataset popularity
  let filename (word "newsite_runs/T_62.asc")
  gis:store-dataset patch_popularity filename
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
953
1204
-1
-1
5.0
1
10
1
1
1
0
0
0
1
0
146
0
236
0
0
1
ticks
30.0

BUTTON
61
21
193
54
Set up Landscape
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
75
63
179
96
Find A* Path
Look-for-goal
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
67
105
190
138
Set Up Traveller
set-up-weighted-walk
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
58
147
200
180
Find Weighted Path
weighted-walk
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
69
235
189
268
Show Popularity
show-popularity
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
60
297
204
342
Turtles reached goal
success-rate
17
1
11

INPUTBOX
1258
18
1413
78
dtp_weight
1.0
1
0
Number

INPUTBOX
1092
83
1247
143
togoal_weight
1.0
1
0
Number

INPUTBOX
1092
18
1247
78
access_weight
2.0
1
0
Number

TEXTBOX
1233
151
1383
169
Weights
11
0.0
1

INPUTBOX
1185
204
1340
264
ticks_limit
1000.0
1
0
Number

BUTTON
86
192
169
225
Save Map
store-output
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
96
432
153
477
ticks
ticks
17
1
11

MONITOR
92
375
156
420
NIL
final-cost
17
1
11

@#$#@#$#@
## WHAT IS IT?

This model aims to model the movement of Noongar travellers across a cost surface representing landscape and cultural variables within Noongar boodjar. It allows the user to explore how different movement decision-making priorities and the input of cultural cognitive maps can impact the path that travellers take. Least-cost path tools in GIS work in a similar way, finding the easiest route on a cost surface. However, GIS models work on the assumption that the entire landscape in perfectly known, and that the traveller will always make the best decision when moving. This model is different as the agent does not know the entire landscape in advance, but instead evaluates the best path only locally, and may not always choose the best route.  This is thought to better mimic human movement more accurately. Parameters within the model can be easily changed, thus allowing for broader exploration. This model is designed for researchers, namely anthropologists and potentially also Noongar knowledge holders, to explore how people moved throughout Southwest Australia in the past.

## HOW IT WORKS

The landscape, start and end points are first inputted into the model. An A* algorithm is then used to navigate around obstacles and provide a path representing the Noongar cognitive map that agents in the simulation can follow.
At each tick, the traveler evaluates the direction in which they need to travel to reach their goal, taking into consideration the accessibility of their surrounding patches and their position in the landscape. At all times, the traveler ha a temporary local target that attempts to bring them closer to the final goal. At the beginning of each tick, the traveler evaluates if they have reached their final goal. If not, the traveler then assesses if its final goal is within its surrounding patches and if it is moves towards it. If the goal is not visible the agent looks at its surrounding 7 patches (Moore neighbourhood) and identify which one it will choose.
At every step, the traveler evaluates its Moore neighbourhood and identifies the suitability of each patch based on its access value, its distance from the goal and its distance to the A* path. A roulette wheel selection is applied, where the traveler has the highest chance of moving towards the goal with the highest suitability value but may choose a lower suitability patch. Once the traveler has moved to its chosen patch, the patch gains a popularity value of 1. Once a patch has been stepped on, it cannot be stepped on again for the simulation. This is so that the agent does not get stuck and continues moving forward toward its goal. The path followed by the traveller is represented by a coloured line.


## HOW TO USE IT
The model should be used following the buttons on the left hand side in a downwards direction:
Set up landscape - sets up the cost surface raster and the start and end points
Find A path - Finds the A* path and calculates final cost of this path
Set up traveller - Sets up the traveller at the start point and sets it a goal
Find Weighted path - The traveller attempts to reach its goal using a weighted random walk function, facilitated by the rnd netlogo extension
Save Map - Once the simulation has finished, the user can export the popularity patches as an ASCII file
Show popularity - visualises the popularity of each patch within the netlogo interface


The monitors show the number of travellers that have reached the goal within a simulation, the final cost of the successful A* path and a tick counter for each run. 

On the right hand side, editable input features are located. Here you can set the weighting for the three attributes of the suitability variable and set a tick limit. 

## THINGS TO NOTICE

Notice how the stochasticity in the model generated through the weighted random walk creates slightly different paths.

## THINGS TO TRY

Play around with the input features to see how changing the weights of each variable and the tick limit changes the path output and the success of the travellers at reaching their goal. 

## EXTENDING THE MODEL

Further parameterisation is required to improve the accuracy of the model. Inclusion of other features (such as a viewshed analysis) may also create interesting results. 

## NETLOGO FEATURES

To run multiple traveller runs within one simulation, the 'ca' function must be manually removed to ensure the popularity counter does not get erased between runs. 

## RELATED MODELS

A similar but more complex model by Gravel-Miguel & Wren (2018) can be found here:
https://www.comses.net/codebases/5782/releases/1.1.0/,


## CREDITS AND REFERENCES

The A* algorithm has been adapted from  Caparrini, F. S. (2018). A General A* Solver in NetLogo. http://www.cs.us.es/~fsancho/?e=131
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="1. Null Test" repetitions="50" runMetricsEveryStep="false">
    <setup>setup
Look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="ticks_limit">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="2. Access - Goal" repetitions="50" runMetricsEveryStep="false">
    <setup>setup
Look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="ticks_limit">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="access_weight">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="3. Goal - Learning" repetitions="50" runMetricsEveryStep="false">
    <setup>setup
Look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="ticks_limit">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_weight">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="4. Short Term Goal" repetitions="50" runMetricsEveryStep="false">
    <setup>setup
Look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="ticks_limit">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="access_weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_weight">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="5. Optimal Route" repetitions="50" runMetricsEveryStep="false">
    <setup>setup
Look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="ticks_limit">
      <value value="25"/>
      <value value="50"/>
      <value value="75"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="access_weight">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="4"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="denmark_AG" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup
look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="togoal_weight">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="access_weight">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ticks_limit">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pop_weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="parameter_testing" repetitions="100" runMetricsEveryStep="false">
    <setup>setup
look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="0"/>
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ticks test" repetitions="1000" runMetricsEveryStep="false">
    <setup>setup
look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <metric>ticks</metric>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="new null model" repetitions="100" runMetricsEveryStep="false">
    <setup>setup
Look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="NULL" repetitions="55" runMetricsEveryStep="false">
    <setup>setup
look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="ACCESS" repetitions="500" runMetricsEveryStep="false">
    <setup>set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <enumeratedValueSet variable="access_weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GOAL" repetitions="60" runMetricsEveryStep="false">
    <setup>setup
look-for-goal
set-up-weighted-walk</setup>
    <go>weighted-walk</go>
    <metric>success-rate</metric>
    <metric>boorna-gnamma-popularity</metric>
    <enumeratedValueSet variable="access_weight">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="togoal_weight">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dtp_weight">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
