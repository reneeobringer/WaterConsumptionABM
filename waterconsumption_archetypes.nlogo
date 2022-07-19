extensions[ array csv profiler time ]

globals [
  ; input variables:
  precip inflow water-use evap losses weather ; weather = for wet or dry days

  ; actual data:
  act-precip act-inflow act-evap act-water-use act-storage-mem act-losses
  variable-table clim-se-table clim-se-list storage

  ; modeled variables:
  mod-storage-mem mod-storage prev-storage

  ; for calculating errors:
  differences nrmse

  ; for time-keeping:
  tick-day month

  ; for scenarios and archetypes
  arch-prob ;scenario
]

turtles-own [ archetype consumption ] ; each turtle is assigned an archetype + a water consumption value

to profile
  setup
  profiler:start
  repeat 3500 [go]
  profiler:stop
  print profiler:report
  profiler:reset
end

to setup
  clear-all
  file-close
  reset-timer
  set tick-day time:anchor-to-ticks (time:create "12-31") 1 "days" ; set timer to start at 12/31 and count up by days
  set variable-table csv:from-file "./inputData/PhoenixWaterBalData.csv"
  set clim-se-table csv:from-file "./inputData/PhoenixClimateSocioEconData.csv"
  set weather [ "dry" ]
  set act-storage-mem []
  set mod-storage-mem []
  set differences []
  ;set scenario "all-changes" ; scenario options: "base", "partial-part", "part", "indiv", "concern", "all-changes"
  create-turtles 4000 [
    set arch-prob random-float 100.0 ; set archetype probability
    assign-archetypes
  ]
  ;get-water-distributions ; calculate water use distributions based on archetypes
  print "Ready!"
  reset-ticks
end

to go
  set month time:get "month" tick-day ; find the month, which will be used to determine the season
  get-input-variables
  daily-storage
  set act-storage-mem fput storage act-storage-mem
  set mod-storage-mem fput mod-storage mod-storage-mem
  set differences fput ( (storage - mod-storage) ^ 2 ) differences
  if (ticks = 3500) [
    calc-errors
    show (word "Model finished in " timer " seconds")
    stop
  ]
  tick
end

to get-input-variables
  let variable-list item ticks variable-table
  set clim-se-list item ticks clim-se-table
  set storage item 0 variable-list        ; actual storage
  set act-precip item 1 variable-list     ; actual preciptiation
  set act-inflow item 2 variable-list     ; actual inflow
  set act-water-use item 3 variable-list  ; actual water use
  set act-evap item 4 variable-list       ; actual evaporation
  set act-losses item 5 variable-list     ; actual losses
  if (ticks = 0) [ set prev-storage item 0 variable-list ]

  ; get the wet/dry conditions (probabilities caclculated using markovchain package in R)
  ;ifelse weather = "wet" [
  ; if random-float 1.000 < 0.393 [ set weather "dry" ] ]    ; probability of getting a dry day after a wet day
  ;[ if random-float 1.000 < 0.095 [ set weather "wet" ] ]   ; probability of getting a wet day after a dry day

  ; get the magnitude of precipitation during a wet day & set precipitation variable & add to the count for continuous dry days (ndd)
  ;ifelse weather = "wet" [
  ;  let p-mag-in-m random-gamma 0.4888451 143.2825410      ; shape and rate parameters calculated in R
  ;  set precip p-mag-in-m * 63885423 ]                     ; multiply magnitude by total surface area of reservoir(s)
  ;[ set precip 0 ]
  set precip act-precip

  ; set the inflow variable (m^3)
  ; set inflow random-gamma 0.16569034181588 0.00000005847319 ; shape and rate parameters calculated in R
  set inflow act-inflow

  ; set the evap variable (m^3)
  ; set evap random-gamma 1.491602174650 0.000003690199       ; shape and rate paramters calculated in R
  set evap act-evap

  ; determine the miscellaneous loss term (m3)
  ; set losses random-normal 1179916 10278650                  ; mean and sd parameters calculated in R
  set losses act-losses

  ; turtles consume water
  ask turtles [
    (ifelse month = 12 or month = 1 or month = 2 or month = 3 [ use-water-winter ]  ; call winter water use distributions
      month = 4 or month = 5 or month = 10 or month = 11 [ use-water-intermediate ] ; call intermediate water use distributions
      month = 6 or month = 7 or month = 8 or month = 9 [ use-water-summer ]         ; call summer water distribution
    )
  ]
  set water-use sum [ consumption ] of turtles * ( 0.5 + random-float 0.5 )         ; total water consumption * a random number between 0.5 and 1.0 to account for the bias towards high water users in the survey
end

to use-water-winter
  (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 2.006613346449 0.000001195737 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.753548850922 0.000001175157 / count turtles ]
      archetype = 3 [ set consumption random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 4 [ set consumption random-gamma 1.737720935896 0.000001143614 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.890994501132 0.000001229424 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.034521643983 0.000001311794 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.985878943549 0.000001328227 / count turtles ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 2.006613346449 0.000001195737 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.753548850922 0.000001175157 / count turtles ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 4 [ set consumption random-gamma 1.737720935896 0.000001143614 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.890994501132 0.000001229424 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.034521643983 0.000001311794 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.985878943549 0.000001328227 / count turtles ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 2.006613346449 0.000001195737 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.753548850922 0.000001175157 / count turtles ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 4 [ set consumption random-gamma 1.737720935896 0.000001143614 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.890994501132 0.000001229424 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.034521643983 0.000001311794 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.985878943549 0.000001328227 / count turtles ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 2.006613346449 0.000001195737 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.753548850922 0.000001175157 / count turtles ]
      archetype = 3 [ set consumption random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 4 [ set consumption random-gamma 1.737720935896 0.000001143614 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.890994501132 0.000001229424 / count turtles ]
      archetype = 6 [ set consumption 0.95 * random-gamma 2.034521643983 0.000001311794 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 1.985878943549 0.000001328227 / count turtles ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 2.006613346449 0.000001195737 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.753548850922 0.000001175157 / count turtles ]
      archetype = 3 [ set consumption random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 4 [ set consumption random-gamma 1.737720935896 0.000001143614 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.890994501132 0.000001229424 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.034521643983 0.000001311794 / count turtles ]
      archetype = 7 [ set consumption random-gamma 1.985878943549 0.000001328227 / count turtles ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 2.006613346449 0.000001195737 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 1.753548850922 0.000001175157 / count turtles ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.0989499037494 0.0000009771535 / count turtles ]
      archetype = 4 [ set consumption random-gamma 1.737720935896 0.000001143614 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 1.890994501132 0.000001229424 / count turtles ]
      archetype = 6 [ set consumption 0.95 * random-gamma 2.034521643983 0.000001311794 / count turtles ]
      archetype = 7 [ set consumption random-gamma 1.985878943549 0.000001328227 / count turtles ]
    ) ]
  )
end

to use-water-intermediate
  (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 2.638905904145 0.000001373255 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.380242486172 0.000001371355 / count turtles ]
      archetype = 3 [ set consumption random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.346537630048 0.000001330988 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.54547023000 0.00000142874 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.733005349687 0.000001523033 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.695228818984 0.000001550195 / count turtles ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 2.638905904145 0.000001373255 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.380242486172 0.000001371355 / count turtles ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.346537630048 0.000001330988 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.54547023000 0.00000142874 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.733005349687 0.000001523033 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.695228818984 0.000001550195 / count turtles ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 2.638905904145 0.000001373255 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.380242486172 0.000001371355 / count turtles ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.346537630048 0.000001330988 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.54547023000 0.00000142874 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.733005349687 0.000001523033 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.695228818984 0.000001550195 / count turtles ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 2.638905904145 0.000001373255 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.380242486172 0.000001371355 / count turtles ]
      archetype = 3 [ set consumption random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.346537630048 0.000001330988 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.54547023000 0.00000142874 / count turtles ]
      archetype = 6 [ set consumption 0.95 * random-gamma 2.733005349687 0.000001523033 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 2.695228818984 0.000001550195 / count turtles ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 2.638905904145 0.000001373255 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.380242486172 0.000001371355 / count turtles ]
      archetype = 3 [ set consumption random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.346537630048 0.000001330988 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.54547023000 0.00000142874 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 2.733005349687 0.000001523033 / count turtles ]
      archetype = 7 [ set consumption random-gamma 2.695228818984 0.000001550195 / count turtles ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 2.638905904145 0.000001373255 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.380242486172 0.000001371355 / count turtles ]
      archetype = 3 [ set consumption 0.75 * random-gamma 1.632164241509 0.000001192973 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.346537630048 0.000001330988 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 2.54547023000 0.00000142874 / count turtles ]
      archetype = 6 [ set consumption 0.95 * random-gamma 2.733005349687 0.000001523033 / count turtles ]
      archetype = 7 [ set consumption random-gamma 2.695228818984 0.000001550195 / count turtles ]
    ) ]
  )
end

to use-water-summer
  (ifelse scenario = "base" [
    (ifelse archetype = 1 [ set consumption random-gamma 3.138162024097 0.000001498654 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.881155983977 0.000001510016 / count turtles ]
      archetype = 3 [ set consumption random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.832204744723 0.000001463409 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.066898019515 0.000001569579 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.289019202043 0.000001672276 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.26212298572 0.00000170704 / count turtles ]
    ) ]
    scenario = "partial-part" [ ; scenario in which some intervention is implemented and 50% of archetype 3 reduce consumption by 25% (become A5)
      let tmp_prob random-float 1
    (ifelse archetype = 1 [ set consumption random-gamma 3.138162024097 0.000001498654 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.881155983977 0.000001510016 / count turtles ]
      archetype = 3 and tmp_prob < 0.5 [ set consumption random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 3 and tmp_prob >= 0.5 [ set consumption 0.75 * random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.832204744723 0.000001463409 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.066898019515 0.000001569579 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.289019202043 0.000001672276 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.26212298572 0.00000170704 / count turtles ]
    ) ]
    scenario = "part" [ ; scenario in which some intervention is implemented and 100% of archetype 3 reduce consumption by 25% (become A5)
    (ifelse archetype = 1 [ set consumption random-gamma 3.138162024097 0.000001498654 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.881155983977 0.000001510016 / count turtles ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.832204744723 0.000001463409 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.066898019515 0.000001569579 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.289019202043 0.000001672276 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.26212298572 0.00000170704 / count turtles ]
    ) ]
    scenario = "indiv" [ ; scenario in which some intervention is implemented and 100% of archetype 6 reduce consumption (become A2)
    (ifelse archetype = 1 [ set consumption random-gamma 3.138162024097 0.000001498654 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.881155983977 0.000001510016 / count turtles ]
      archetype = 3 [ set consumption random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.832204744723 0.000001463409 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.066898019515 0.000001569579 / count turtles ]
      archetype = 6 [ set consumption 0.95 * random-gamma 3.289019202043 0.000001672276 / count turtles ]
      archetype = 7 [ set consumption 1.15 * random-gamma 3.26212298572 0.00000170704 / count turtles ]
    ) ]
    scenario = "concern" [ ; scenario in which some intervention is implemented and 100% of archtype 7 reduce consumption (become A4)
    (ifelse archetype = 1 [ set consumption random-gamma 3.138162024097 0.000001498654 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.881155983977 0.000001510016 / count turtles ]
      archetype = 3 [ set consumption random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.832204744723 0.000001463409 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.066898019515 0.000001569579 / count turtles ]
      archetype = 6 [ set consumption 1.25 * random-gamma 3.289019202043 0.000001672276 / count turtles ]
      archetype = 7 [ set consumption random-gamma 3.26212298572 0.00000170704 / count turtles ]
    ) ]
    scenario = "all-changes" [ ; scenario in which all of the above interventions are applied
    (ifelse archetype = 1 [ set consumption random-gamma 3.138162024097 0.000001498654 / count turtles ] ; shape and rate parameters calculated in R
      archetype = 2 [ set consumption 0.95 * random-gamma 2.881155983977 0.000001510016 / count turtles ]
      archetype = 3 [ set consumption 0.75 * random-gamma 2.073043783342 0.000001345703 / count turtles ]
      archetype = 4 [ set consumption random-gamma 2.832204744723 0.000001463409 / count turtles ]
      archetype = 5 [ set consumption 0.75 * random-gamma 3.066898019515 0.000001569579 / count turtles ]
      archetype = 6 [ set consumption 0.95 * random-gamma 3.289019202043 0.000001672276 / count turtles ]
      archetype = 7 [ set consumption random-gamma 3.26212298572 0.00000170704 / count turtles ]
    ) ]
  )
end

to assign-archetypes ; set archetype based on previously determined proportions (Obringer and White, under review)
        (ifelse arch-prob <= 19.4 [ set archetype 1 ]
          arch-prob > 19.4 and arch-prob <= 38.5 [ set archetype 2 ]
          arch-prob > 38.5 and arch-prob <= 63.1 [ set archetype 3 ]
          arch-prob > 63.1 and arch-prob <= 76.4 [ set archetype 4 ]
          arch-prob > 76.4 and arch-prob <= 94.8 [ set archetype 5 ]
          arch-prob > 94.8 and arch-prob <= 98.7 [ set archetype 6 ]
          arch-prob > 98.7 [ set archetype 7 ]
        )
end

to daily-storage
  set mod-storage (prev-storage + precip + inflow - water-use - evap - losses)
  set prev-storage mod-storage
end

to calc-errors
  set nrmse ( sqrt ( ( sum differences ) / 3900  ) ) / ( max act-storage-mem - min act-storage-mem )
  output-print word "NRMSE: " nrmse
end
@#$#@#$#@
GRAPHICS-WINDOW
11
10
924
724
-1
-1
5.0
1
10
1
1
1
0
1
1
1
-90
90
-70
70
0
0
1
ticks
30.0

BUTTON
991
83
1063
116
GO
go
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
992
33
1062
66
SETUP
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

TEXTBOX
1375
10
1525
30
INPUTS
16
0.0
1

TEXTBOX
1372
389
1522
409
OUTPUTS
16
0.0
1

PLOT
1094
412
1741
647
storage
time
storage
0.0
10.0
0.0
4.0
true
true
"" ""
PENS
"Model" 1.0 0 -5298144 true "" "plot mod-storage"
"Actual" 1.0 0 -7500403 true "" "plot storage"

PLOT
1093
34
1293
184
precip
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -11221820 true "" "plot precip"
"pen-1" 1.0 0 -7500403 true "" "plot act-precip"

PLOT
1312
35
1512
185
inflow
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -14439633 true "" "plot inflow"
"pen-1" 1.0 0 -7500403 true "" "plot act-inflow"

PLOT
1539
35
1739
185
evap
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -4079321 true "" "plot evap"
"pen-1" 1.0 0 -7500403 true "" "plot act-evap"

OUTPUT
1095
663
1335
717
13

BUTTON
992
135
1063
168
PROFILE
profile
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1099
210
1299
360
wateruse
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13791810 true "" "plot water-use"
"pen-1" 1.0 0 -7500403 true "" "plot act-water-use"

CHOOSER
1334
214
1472
259
scenario
scenario
"base" "partial-part" "part" "indiv" "concern" "all-changes"
2

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="multi-run" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3501"/>
    <metric>mod-storage</metric>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;base&quot;"/>
      <value value="&quot;partial-part&quot;"/>
      <value value="&quot;part&quot;"/>
      <value value="&quot;indiv&quot;"/>
      <value value="&quot;concern&quot;"/>
      <value value="&quot;all-changes&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="multi-run_manual_scenarios" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="3501"/>
    <metric>mod-storage</metric>
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
