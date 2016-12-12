;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GNU GENERAL PUBLIC LICENSE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CharityWorld-JASSS
;; CharityWorld-JASSS is a model designed to show the emergent effects 
;; of floating-point errors in agent-based models.
;; Copyright (C) 2005 Luis R. Izquierdo & J. Gary Polhill
;; 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;
;; Contact information:
;; Luis R. Izquierdo 
;;   University of Burgos, Spain. 
;;   e-mail: lrizquierdo@ubu.es

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DECLARATION OF VARIABLES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

globals [ 
  agents
    ;; This is an agentset containing all the patches in the
    ;; four quadrants (it excludes the patches that divide the 
    ;; grid into the four quadrants)
    
    ;; The following are the 4 types of agents.
    ;; Each type is in a different quadrant.
    ;; These variables are agentsets.
  inclusive-total-agents 
  inclusive-mean-agents
  exclusive-total-agents
  exclusive-mean-agents
  
  time-step 
  my-random-seed
]

patches-own [  ;; patches are the agents. We do not use turtles here
  wealth
    ;; The following 3 variables are boolean
    ;; At any particular time, each agent can have
    ;; only one of those set to true
  locally-rich?
  locally-average?
  locally-poor?
  my-neighbours 
    ;; In general, these are the 8 neighbours 
    ;; in the Moore neighbourhood of radius 1
    ;; but there is no wrap-around.
  my-neighbours-neighbours
    ;; This is an agentset formed by my neighbours'
    ;; neighbours (so, in particular, it includes
    ;; my neighbours and myself). 
    ;; This is defined for efficiency reasons.
]

;;;;;;;;;;;;;;;;;;;;;;;
;;; CORE PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to startup
    ;; This procedure is called when the model is first loaded
  setup
end

to setup
  clear-all
  set my-random-seed random 1.0E9
  random-seed my-random-seed
  setup-patches
  set time-step 0
  update-graphs 
end

to setup-patches
  set agents patches with [ pxcor != 0 and pycor != 0 ]
    ;; Exclude the boundaries between quadrants
    
    ;; Set up the neighbourhoods (no wrap-around)
  ask agents [
    set my-neighbours values-from 
      (neighbors with [
        pxcor != 0 and pycor != 0 and
        abs (pxcor - pxcor-of myself) <= 1 and
        abs (pycor - pycor-of myself) <= 1]) 
       [ self ]
  ]
  
    ;; Create the list of my neighbours' neighbours...
  ask agents [
    set my-neighbours-neighbours 
      remove-duplicates reduce [sentence ?1 ?2] 
        map [my-neighbours-of ? ] my-neighbours  
  ]
    ;; ...and turn it into an agentset (for efficiency)
  ask agents [
    set my-neighbours-neighbours 
      (agents in-radius-no-wrap 3) with [ 
        member? myself my-neighbours-neighbours]
  ] ;; (note that to be a neighbour's neighbour is commutative)
  
    ;; Give 2 coins to everyone
  ask agents [ 
    set wealth coin-value 
    set wealth (wealth + coin-value) ] 
  
  ask agents [determine-status]
  
  set inclusive-total-agents agents with [ pxcor < 0 and pycor > 0 ]
  set inclusive-mean-agents  agents with [ pxcor < 0 and pycor < 0 ]
  set exclusive-total-agents agents with [ pxcor > 0 and pycor > 0 ]
  set exclusive-mean-agents  agents with [ pxcor > 0 and pycor < 0 ]
end

to conduct-lottery
    ;; Everyone gives two coins to the agent in their centre
  let winner-x ceiling (screen-edge-x / 2)
  let winner-y ceiling (screen-edge-y / 2)
  
  ask inclusive-total-agents [ 
    give-one-coin-to patch (- winner-x) winner-y 
    give-one-coin-to patch (- winner-x) winner-y 
  ]
  ask inclusive-mean-agents [ 
    give-one-coin-to patch (- winner-x) (- winner-y)
    give-one-coin-to patch (- winner-x) (- winner-y) 
  ]
  ask exclusive-total-agents [ 
    give-one-coin-to patch winner-x winner-y
    give-one-coin-to patch winner-x winner-y 
  ]
  ask exclusive-mean-agents [ 
    give-one-coin-to patch winner-x (- winner-y)
    give-one-coin-to patch winner-x (- winner-y) 
  ]   
      
  ask agents [determine-status] 
  update-graphs
end

to go
  no-display
  
  let time-step-random-seed random 1.0E9
  
  random-seed time-step-random-seed
    sub-go inclusive-total-agents
  random-seed time-step-random-seed
    sub-go inclusive-mean-agents
  random-seed time-step-random-seed
    sub-go exclusive-total-agents
  random-seed time-step-random-seed
    sub-go exclusive-mean-agents
    
  update-graphs
  display
  
  if not any? (agents with [ locally-rich? = true ]) [ 
    user-message "No more locally rich agents\n" +
                 "in any implementation.\n" +
                 "Cycles conducted: " + time-step
    stop 
  ]
   
  set time-step (time-step + 1)
  
end

to sub-go [agentset]
  let rich-agents agentset with [ locally-rich? = true ] 
    ;; Identify the agents who are locally rich
  if any? rich-agents [   
      ;; If there's any rich agents, choose one of them at random
      ;; and tell it to conduct one cycle of redistribution
    ask random-one-of rich-agents [conduct-cycle-of-redistribution] 
  ]
end

to conduct-cycle-of-redistribution 
    ;; Give one coin to each of your neighbours in ascending order of wealth as long as:
    ;;  1. You are locally rich
    ;;  2. Your neighbour isn't
  set my-neighbours shuffle my-neighbours 
    ;; Make sure there's no bias in ties when we sort the list
  set my-neighbours sort-by-wealth my-neighbours 
    ;; Sort the list in ascending order of wealth
  foreach my-neighbours [
    determine-status
    ask ? [determine-status]
    if locally-rich? and (not locally-rich?-of ?) [ 
      give-one-coin-to ?      
    ]   
  ]
  
    ;; A neccessary condition for a change of status to happen is
    ;; that your wealth or one of your neighbours' wealth has changed.
    ;; Thus, I only have to ask my neighbours' neighbours to determine
    ;; their status. Nobody else's status may have changed.
  ask my-neighbours-neighbours [determine-status]
  
end

to give-one-coin-to [ recipient ]
  set wealth (wealth - coin-value)
  set wealth-of recipient (wealth-of recipient + coin-value)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROCEDURE TO DETERMINE WHETHER AN AGENT IS LOCALLY RICH, AVERAGE, OR POOR ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to determine-status
  ;; Four different ways of assessing whether an agent is 
  ;; locally rich, average, or poor. The four of them are 
  ;; mathematically equivalent in real arithmetic, 
  ;; but they can give different results in floating-point arithmetic.
  
  ifelse pxcor < 0 
  [   ;; inclusive
    ifelse pycor > 0 
    [ ;; inclusive-total
      let nbrTotal (sum map [wealth-of ?] my-neighbours) + wealth
      let super-wealth ((length my-neighbours) + 1) * wealth
      set locally-rich? (greater-than super-wealth nbrTotal)
      set locally-average? (equal-to super-wealth nbrTotal)
      set locally-poor? (less-than super-wealth nbrTotal)
    ]
    [ ;; inclusive-mean
      let nbrMean ((sum map [wealth-of ?] my-neighbours) + wealth) / ((length my-neighbours) + 1)
      set locally-rich? (greater-than wealth nbrMean)
      set locally-average? (equal-to wealth nbrMean)
      set locally-poor? (less-than wealth nbrMean)
    ]  
  ]
  [   ;; exclusive
    ifelse pycor > 0 
    [ ;; exclusive-total
      let nbrTotal (sum map [wealth-of ?] my-neighbours)
      let super-wealth (length my-neighbours) * wealth
      set locally-rich? (greater-than super-wealth nbrTotal)
      set locally-average? (equal-to super-wealth nbrTotal)
      set locally-poor? (less-than super-wealth nbrTotal)
    ]
    [ ;; exclusive-mean
      let nbrMean (sum map [wealth-of ?] my-neighbours) / (length my-neighbours)
      set locally-rich? (greater-than wealth nbrMean)
      set locally-average? (equal-to wealth nbrMean)
      set locally-poor? (less-than wealth nbrMean)    
    ]
  ] 
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPARISON OPERATORS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

to-report greater-than [a b]
  if (Technique-to-deal-with-fp-errors = "None") [
    report ifelse-value (a > b) [true] [false]
  ]
  if (Technique-to-deal-with-fp-errors = "Tolerance Windows (epsilon)") [
    report ifelse-value (a > b + epsilon) [true] [false]
  ]
  if (Technique-to-deal-with-fp-errors = "Round to n significant digits") [
    report ifelse-value 
      ((round-to-digits a significant-digits) > (round-to-digits b significant-digits)) 
      [true] [false]
  ]
end

to-report less-than [a b]
  if (Technique-to-deal-with-fp-errors = "None") [
    report ifelse-value (a < b) [true] [false]
  ]
  if (Technique-to-deal-with-fp-errors = "Tolerance Windows (epsilon)") [
    report ifelse-value (a < b - epsilon) [true] [false]
  ]
  if (Technique-to-deal-with-fp-errors = "Round to n significant digits") [
    report ifelse-value 
      ((round-to-digits a significant-digits) < (round-to-digits b significant-digits)) 
      [true] [false]
  ]
end

to-report equal-to [a b]
  if (Technique-to-deal-with-fp-errors = "None") [
    report ifelse-value (a = b) [true] [false]
  ]
  if (Technique-to-deal-with-fp-errors = "Tolerance Windows (epsilon)") [
    report ifelse-value 
      ((a >= b - epsilon) AND (a <= b + epsilon)) [true] [false]    
  ]
  if (Technique-to-deal-with-fp-errors = "Round to n significant digits") [
    report ifelse-value 
      ((round-to-digits a significant-digits) = (round-to-digits b significant-digits)) 
      [true] [false]
  ]
end

to-report round-to-digits [number digits]
  if ((number >= (- epsilon)) AND (number <= epsilon)) [report 0.0]
  let power-of-ten ((ceiling (log (abs number) 10)) - digits)
  if-else power-of-ten > 0
    ;; Be careful with round, as it returns an integer (very short range).
    [ report (round (number / (10.0 ^ power-of-ten))) * (10.0 ^ power-of-ten) ]
    [ report (round (number * (10.0 ^ (- power-of-ten)))) / (10.0 ^ (- power-of-ten)) ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAPH-RELATED PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to update-graphs
  update-grid
  
  set-current-plot "Inclusive Total"
  update-plot-with inclusive-total-agents 
  
  set-current-plot "Inclusive Mean"
  update-plot-with inclusive-mean-agents
  
  set-current-plot "Exclusive Total"
  update-plot-with exclusive-total-agents
  
  set-current-plot "Exclusive Mean"  
  update-plot-with exclusive-mean-agents
 
end
  
to update-grid
  ifelse display-mode = "Absolute wealth" 
    [ ask agents [update-colour-absolute] ]
    [ ask agents [update-colour-relative] ]
end

to update-colour-relative 
  if locally-rich? [ set pcolor turquoise  stop]
  if locally-average? [ set pcolor orange  stop]
  if locally-poor? [ set pcolor magenta ]
end

to update-colour-absolute
  if wealth > 2 * coin-value [ 
    ifelse wealth > (2 * screen-size-x * screen-size-y)
      [set pcolor 50] ;; this should not happen if there's only one lottery
      [set pcolor 
        (50 + 7 * (((2 * screen-size-x * screen-size-y) - wealth) / 
          (2 * screen-size-x * screen-size-y - 2 * coin-value)) ^ 30)]
            ;; I raise to the power of 30 to make the colour scale finer 
            ;; when the wealth is close to 2 coins
      
  ]
  if wealth < 2 * coin-value [ 
    ifelse wealth < 0 
      [set pcolor red] ;; this should not happen if there's only one lottery
      [set pcolor (15 + 5 * wealth / (2 * coin-value))] 
  ]
  ifelse wealth = 2 * coin-value 
    [ set pcolor 47 ]
    [ if wealth > 1.5 * coin-value and wealth < 2.5 * coin-value [ set pcolor blue ] ]
      ;; This final condition, implemented to detect floating-point errors, 
      ;; overrides the previous conditions.
end

  ;; Note that the first two points that we draw correspond
  ;; to time-step 0.
to update-plot-with [ agentset ]
  set-current-plot-pen "locally-rich" 
  plot count agentset with [locally-rich? = true]
  set-current-plot-pen "locally-average" 
  plot count agentset with [locally-average? = true]
  set-current-plot-pen "locally-poor" 
  plot count agentset with [locally-poor? = true]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RANDOM SEED RELATED PROCEDURE ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use a seed entered by the user
to use-seed-from-user
  set my-random-seed read-from-string user-input "Enter a random seed (an integer):"
  random-seed my-random-seed   ;; use the new seed
end

;;;;;;;;;;;;;;;;;;;;;;;
;; SORTING ALGORITHM ;;
;;;;;;;;;;;;;;;;;;;;;;;

to-report sort-by-wealth [list-of-agents]
  ;; In principle, this could easily be done just by writing:
  ;;
  ;; sort-by [ wealth-of ?1 < wealth-of ?2 ] list-of-agents
  ;;
  ;; This, however, would represent a problem: consider a simulation 
  ;; with floating-point errors where some agents that would have
  ;; the same wealth in the absence of errors have different wealth.
  ;; Now imagine that we use a certain technique to deal with 
  ;; floating-point errors that would amend this. This technique,
  ;; however will not be used when using sort-by. Because of this, 
  ;; we have to implement our own sorting algorithm. 
  ;;
  ;; The following algorithm (straight insertion) is an 
  ;; N^2 routine (for arrays), and it should only be used for small N, 
  ;; say < 20. Alternative algorithms for larger lists are 
  ;; Shell's Method (N^(3/2)), and the fastest in average 
  ;; Quicksort (though it is N^2 for the worst case!).
  ;; See "Numerical Recipes in C : The Art of Scientific Computing", 
  ;; by William H. Press, Brian P. Flannery, Saul A. Teukolsky, 
  ;; William T. Vetterling. Cambridge University Press; 
  ;; 2 edition (October 30, 1992).

  let picked-position 1
  repeat (length list-of-agents - 1) [  
    let picked-agent (item picked-position list-of-agents)
    let value (wealth-of picked-agent) 
    let counter (picked-position - 1)
    while [ counter >= 0 AND (greater-than (wealth-of (item counter list-of-agents)) value) ]
      [ set list-of-agents 
          (replace-item (counter + 1) list-of-agents (item counter list-of-agents))
        set counter (counter - 1)
      ]
    set list-of-agents 
        (replace-item (counter + 1) list-of-agents picked-agent)
    set picked-position (picked-position + 1)  
  ]
  report list-of-agents
end
@#$#@#$#@
GRAPHICS-WINDOW
241
101
548
429
13
13
11.0
1
10
1
1
1
0
1
1
1

CC-WINDOW
5
570
794
665
Command Center
0

BUTTON
5
51
87
84
Start over
setup
NIL
1
T
OBSERVER
T
NIL

BUTTON
530
10
785
43
Conduct cycles of redistribution repeatedly
go
T
1
T
OBSERVER
NIL
NIL

SLIDER
5
10
177
43
coin-value
coin-value
0.1
10
1.0
0.1
1
NIL

PLOT
550
117
785
269
Exclusive Total
Time-step
Count
0.0
10.0
0.0
10.0
true
false
PENS
"locally-rich" 1.0 0 -14835848 true
"locally-average" 1.0 0 -955883 true
"locally-poor" 1.0 0 -5825686 true

BUTTON
624
47
785
80
One cycle of redistribution
go
NIL
1
T
OBSERVER
T
NIL

BUTTON
390
10
498
43
Conduct lottery
conduct-lottery
NIL
1
T
OBSERVER
T
NIL

MONITOR
550
47
620
96
cycles
time-step
0
1

TEXTBOX
92
50
211
95
When you click here, every agent starts with two coins

TEXTBOX
378
47
514
94
When you click here, every agent gives two coins to the agent in the centre

PLOT
550
270
785
427
Exclusive Mean
Time-step
Count
0.0
10.0
0.0
10.0
true
false
PENS
"locally-rich" 1.0 0 -14835848 true
"locally-average" 1.0 0 -955883 true
"locally-poor" 1.0 0 -5825686 true

PLOT
4
115
239
268
Inclusive Total
Time-step
Count
0.0
10.0
0.0
10.0
true
false
PENS
"locally-rich" 1.0 0 -14835848 true
"locally-average" 1.0 0 -955883 true
"locally-poor" 1.0 0 -5825686 true

MONITOR
241
47
330
96
Random seed
my-random-seed
0
1

BUTTON
218
10
357
43
Change random seed
use-seed-from-user
NIL
1
T
OBSERVER
T
NIL

PLOT
4
269
239
426
Inclusive Mean
Time-step
Count
0.0
10.0
0.0
10.0
true
false
PENS
"locally-rich" 1.0 0 -14835848 true
"locally-average" 1.0 0 -955883 true
"locally-poor" 1.0 0 -5825686 true

TEXTBOX
6
430
395
506
Legend for grid in "Absolute wealth" display mode:\n  Green shades: wealth > 2 coins\n  Yellow:             wealth = 2 coins\n  Red shades:     wealth < 2 coins\n  Blue overrides when: wealth != 2 coins AND (1.5 coins < wealth < 2.5 coins)

TEXTBOX
552
430
784
505
Legend for time-series graphs, and for the grid in "Relative wealth" display mode:\n  Turquoise:   locally rich \n  Orange:       locally average\n  Magenta:     locally poor

CHOOSER
289
432
429
477
display-mode
display-mode
"Absolute wealth" "Relative wealth"
0

BUTTON
433
432
528
465
Update grid
update-grid
NIL
1
T
OBSERVER
T
NIL

CHOOSER
118
511
319
556
Technique-to-deal-with-fp-errors
Technique-to-deal-with-fp-errors
"None" "Tolerance Windows (epsilon)" "Round to n significant digits"
0

SLIDER
331
511
503
544
epsilon
epsilon
0
2
0.0010
1.0E-4
1
NIL

SLIDER
515
511
687
544
significant-digits
significant-digits
1
9
6
1
1
NIL

@#$#@#$#@

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
