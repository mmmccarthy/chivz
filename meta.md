About the Data
--------------

This app pulls the [Traffic Crashes
dataset](https://data.cityofchicago.org/Transportation/Traffic-Crashes-Crashes/85ca-t3if)
from Chicago's data portal, which is updated frequently with new crash
reports from the Chicago Police Department. Each row represents a crash
event, which may involve multiple vehicles, cyclists, or pedestrians
(called "vehicles" or "units") and multiple involved people.

::: {.figure}
```{=html}
<!--html_preserve-->
```
::: {#htmlwidget-040d05df72d4ce5c1a2b .grViz .html-widget style="width:672px;height:480px;"}
:::

```{=html}
<script type="application/json" data-for="htmlwidget-040d05df72d4ce5c1a2b">{"x":{"diagram":"digraph g {\n  graph [rankdir = \"LR\"]\n  \n  node [fontsize = \"16\" shape=\"record\" fontname = \"Helvetica\"];\n  \"crashes\" [label = \"Crashes | rd_no | crash_date | <f0> crash_record_id | num_units | most_severe_injury | injuries_total | latitude | longitude\"];\n  \n  \"vehicles\" [label = \"Vehicles |<f0> crash_unit_id |<f1> crash_record_id | unit_no | unit_type | num_passengers | vehicle_use | DOT_no | ILCC_no\"];\n  \n  \"people\" [label = \"People | person_id | person_type |<f0> crash_record_id |<f1> vehicle_id | seat_no | age | sex | airbag_deployed | injury_classification\"];\n  \n  edge [arrowhead = \"none\"];\n  \"crashes\":f0 -> \"vehicles\":f1;\n  \"crashes\":f0 -> \"people\":f0;\n  \"vehicles\":f0 -> \"people\":f1;\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```
```{=html}
<!--/html_preserve-->
```
```{=html}
<p class="caption">
```
Figure 1: Diagram of crash table IDs
```{=html}
</p>
```
:::

Each Crash row can be joined to related rows in the Vehicles and People
tables, as shown in Figure 1. For example, a crash between two motor
vehicles will have one Crash row, two Vehicle rows, and a People row for
each occupant. A Pedestrian or Cyclist involved in a crash has both a
`Vehicle` record and a `People` record.

Before the city published this open dataset in 2018, crash report
summaries were only available through the Illinois Department of
Transportation (IDOT). IDOT recieved crash reports from law enforcement
agencies across the state and compiled these reports into a database.
IDOT only provided the resulting data upon request, often with a year
lag between the end of the reporting year and when data was available.
As part of Vision Zero, the city committed to shifting to an electronic
crash report system that would allow for publishing crash summaries
closer to real-time.

Data Dictionary
---------------

### Crashes table

  --------------------------------------------------------------------------------------------------
  Field                              Type             Description
  ---------------------------------- ---------------- ----------------------------------------------
  crash\_record\_id                  text             This number can be used to link to the same
                                                      crash in the Vehicles and People datasets.
                                                      This number also serves as a unique ID in this
                                                      dataset.

  rd\_no                             text             Chicago Police Department report number. For
                                                      privacy reasons, this column is blank for
                                                      recent crashes.

  crash\_date\_est\_i                text             Crash date estimated by desk officer or
                                                      reporting party (only used in cases where
                                                      crash is reported at police station days after
                                                      the crash)

  crash\_date                        calendar\_date   Date and time of crash as entered by the
                                                      reporting officer

  posted\_speed\_limit               number           Posted speed limit, as determined by reporting
                                                      officer

  traffic\_control\_device           text             Traffic control device present at crash
                                                      location, as determined by reporting officer

  device\_condition                  text             Condition of traffic control device, as
                                                      determined by reporting officer

  weather\_condition                 text             Weather condition at time of crash, as
                                                      determined by reporting officer

  lighting\_condition                text             Light condition at time of crash, as
                                                      determined by reporting officer

  first\_crash\_type                 text             Type of first collision in crash

  trafficway\_type                   text             Trafficway type, as determined by reporting
                                                      officer

  lane\_cnt                          number           Total number of through lanes in either
                                                      direction, excluding turn lanes, as determined
                                                      by reporting officer (0 = intersection)

  alignment                          text             Street alignment at crash location, as
                                                      determined by reporting officer

  roadway\_surface\_cond             text             Road surface condition, as determined by
                                                      reporting officer

  road\_defect                       text             Road defects, as determined by reporting
                                                      officer

  report\_type                       text             Administrative report type (at scene, at desk,
                                                      amended)

  crash\_type                        text             A general severity classification for the
                                                      crash. Can be either Injury and/or Tow Due to
                                                      Crash or No Injury / Drive Away

  intersection\_related\_i           text             A field observation by the police officer
                                                      whether an intersection played a role in the
                                                      crash. Does not represent whether or not the
                                                      crash occurred within the intersection.

  private\_property\_i               text             Whether the crash begun or first contact was
                                                      made outside of the public right-of-way.

  hit\_and\_run\_i                   text             Crash did/did not involve a driver who caused
                                                      the crash and fled the scene without
                                                      exchanging information and/or rendering aid

  damage                             text             A field observation of estimated damage.

  date\_police\_notified             calendar\_date   Calendar date on which police were notified of
                                                      the crash

  prim\_contributory\_cause          text             The factor which was most significant in
                                                      causing the crash, as determined by officer
                                                      judgment

  sec\_contributory\_cause           text             The factor which was second most significant
                                                      in causing the crash, as determined by officer
                                                      judgment

  street\_no                         number           Street address number of crash location, as
                                                      determined by reporting officer

  street\_direction                  text             Street address direction (N,E,S,W) of crash
                                                      location, as determined by reporting officer

  street\_name                       text             Street address name of crash location, as
                                                      determined by reporting officer

  beat\_of\_occurrence               number           Chicago Police Department Beat ID. Boundaries
                                                      available at
                                                      <https://data.cityofchicago.org/d/aerh-rz74>

  photos\_taken\_i                   text             Whether the Chicago Police Department took
                                                      photos at the location of the crash

  statements\_taken\_i               text             Whether statements were taken from unit(s)
                                                      involved in crash

  dooring\_i                         text             Whether crash involved a motor vehicle
                                                      occupant opening a door into the travel path
                                                      of a bicyclist, causing a crash

  work\_zone\_i                      text             Whether the crash occurred in an active work
                                                      zone

  work\_zone\_type                   text             The type of work zone, if any

  workers\_present\_i                text             Whether construction workers were present in
                                                      an active work zone at crash location

  num\_units                         number           Number of units involved in the crash. A unit
                                                      can be a motor vehicle, a pedestrian, a
                                                      bicyclist, or another non-passenger roadway
                                                      user. Each unit represents a mode of traffic
                                                      with an independent trajectory.

  most\_severe\_injury               text             Most severe injury sustained by any person
                                                      involved in the crash

  injuries\_total                    number           Total persons sustaining fatal,
                                                      incapacitating, non-incapacitating, and
                                                      possible injuries as determined by the
                                                      reporting officer

  injuries\_fatal                    number           Total persons sustaining fatal injuries in the
                                                      crash

  injuries\_incapacitating           number           Total persons sustaining
                                                      incapacitating/serious injuries in the crash
                                                      as determined by the reporting officer. Any
                                                      injury other than fatal injury, which prevents
                                                      the injured person from walking, driving, or
                                                      normally continuing the activities they were
                                                      capable of performing before the injury
                                                      occurred. Includes severe lacerations, broken
                                                      limbs, skull or chest injuries, and abdominal
                                                      injuries.

  injuries\_non\_incapacitating      number           Total persons sustaining non-incapacitating
                                                      injuries in the crash as determined by the
                                                      reporting officer. Any injury, other than
                                                      fatal or incapacitating injury, which is
                                                      evident to observers at the scene of the
                                                      crash. Includes lump on head, abrasions,
                                                      bruises, and minor lacerations.

  injuries\_reported\_not\_evident   number           Total persons sustaining possible injuries in
                                                      the crash as determined by the reporting
                                                      officer. Includes momentary unconsciousness,
                                                      claims of injuries not evident, limping,
                                                      complaint of pain, nausea, and hysteria.

  injuries\_no\_indication           number           Total persons sustaining no injuries in the
                                                      crash as determined by the reporting officer

  injuries\_unknown                  number           Total persons for whom injuries sustained, if
                                                      any, are unknown

  crash\_hour                        number           The hour of the day component of CRASH\_DATE.

  crash\_day\_of\_week               number           The day of the week component of CRASH\_DATE.
                                                      Sunday=1

  crash\_month                       number           The month component of CRASH\_DATE.

  latitude                           number           The latitude of the crash location, as
                                                      determined by reporting officer, as derived
                                                      from the reported address of crash

  longitude                          number           The longitude of the crash location, as
                                                      determined by reporting officer, as derived
                                                      from the reported address of crash

  location                           point            The crash location, as determined by reporting
                                                      officer, as derived from the reported address
                                                      of crash, in a column type that allows for
                                                      mapping and other geographic analysis in the
                                                      data portal software
  --------------------------------------------------------------------------------------------------

### Vehicles table

  ------------------------------------------------------------------------------
  Field                          Type             Description
  ------------------------------ ---------------- ------------------------------
  crash\_unit\_id                number           A unique identifier for each
                                                  vehicle record.

  crash\_record\_id              text             This number can be used to
                                                  link to the same crash in the
                                                  Crashes and People datasets.
                                                  This number also serves as a
                                                  unique ID in the Crashes
                                                  dataset.

  rd\_no                         text             Chicago Police Department
                                                  report number. For privacy
                                                  reasons, this column is blank
                                                  for recent crashes.

  crash\_date                    calendar\_date   Date and time of crash as
                                                  entered by the reporting
                                                  officer

  unit\_no                       number           A unique ID for each unit
                                                  within a specific crash
                                                  report.

  unit\_type                     text             The type of unit

  num\_passengers                number           Number of passengers in the
                                                  vehicle. The driver is not
                                                  included. More information on
                                                  passengers is in the People
                                                  dataset.

  vehicle\_id                    number           

  cmrc\_veh\_i                   text             

  make                           text             The make (brand) of the
                                                  vehicle, if relevant

  model                          text             The model of the vehicle, if
                                                  relevant

  lic\_plate\_state              text             The state issuing the license
                                                  plate of the vehicle, if
                                                  relevant

  vehicle\_year                  number           The model year of the vehicle,
                                                  if relevant

  vehicle\_defect                text             

  vehicle\_type                  text             The type of vehicle, if
                                                  relevant

  vehicle\_use                   text             The normal use of the vehicle,
                                                  if relevant

  travel\_direction              text             The direction in which the
                                                  unit was traveling prior to
                                                  the crash, as determined by
                                                  the reporting officer

  maneuver                       text             The action the unit was taking
                                                  prior to the crash, as
                                                  determined by the reporting
                                                  officer

  towed\_i                       text             Indicator of whether the
                                                  vehicle was towed

  fire\_i                        text             

  occupant\_cnt                  number           The number of people in the
                                                  unit, as determined by the
                                                  reporting officer

  exceed\_speed\_limit\_i        text             Indicator of whether the unit
                                                  was speeding, as determined by
                                                  the reporting officer

  towed\_by                      text             Entity that towed the unit, if
                                                  relevant

  towed\_to                      text             Location to which the unit was
                                                  towed, if relevant

  area\_00\_i                    text             

  area\_01\_i                    text             

  area\_02\_i                    text             

  area\_03\_i                    text             

  area\_04\_i                    text             

  area\_05\_i                    text             

  area\_06\_i                    text             

  area\_07\_i                    text             

  area\_08\_i                    text             

  area\_09\_i                    text             

  area\_10\_i                    text             

  area\_11\_i                    text             

  area\_12\_i                    text             

  area\_99\_i                    text             

  first\_contact\_point          text             

  cmv\_id                        number           

  usdot\_no                      text             

  ccmc\_no                       text             

  ilcc\_no                       text             

  commercial\_src                text             

  gvwr                           text             

  carrier\_name                  text             

  carrier\_state                 text             

  carrier\_city                  text             

  hazmat\_placards\_i            text             

  hazmat\_name                   text             

  un\_no                         text             

  hazmat\_present\_i             text             

  hazmat\_report\_i              text             

  hazmat\_report\_no             text             

  mcs\_report\_i                 text             

  mcs\_report\_no                text             

  hazmat\_vio\_cause\_crash\_i   text             

  mcs\_vio\_cause\_crash\_i      text             

  idot\_permit\_no               text             

  wide\_load\_i                  text             

  trailer1\_width                text             

  trailer2\_width                text             

  trailer1\_length               number           

  trailer2\_length               number           

  total\_vehicle\_length         number           

  axle\_cnt                      number           

  vehicle\_config                text             

  cargo\_body\_type              text             

  load\_type                     text             

  hazmat\_out\_of\_service\_i    text             

  mcs\_out\_of\_service\_i       text             

  hazmat\_class                  text             
  ------------------------------------------------------------------------------

### People table

  ---------------------------------------------------------------------------
  Field                     Type             Description
  ------------------------- ---------------- --------------------------------
  person\_id                text             A unique identifier for each
                                             person record. IDs starting with
                                             P indicate passengers. IDs
                                             starting with O indicate a
                                             person who was not a passenger
                                             in the vehicle (e.g., driver,
                                             pedestrian, cyclist, etc.).

  person\_type              text             Type of roadway user involved in
                                             crash

  crash\_record\_id         text             This number can be used to link
                                             to the same crash in the Crashes
                                             and Vehicles datasets. This
                                             number also serves as a unique
                                             ID in the Crashes dataset.

  rd\_no                    text             Chicago Police Department report
                                             number. For privacy reasons,
                                             this column is blank for recent
                                             crashes.

  vehicle\_id               text             The corresponding
                                             CRASH\_UNIT\_ID from the
                                             Vehicles dataset.

  crash\_date               calendar\_date   Date and time of crash as
                                             entered by the reporting officer

  seat\_no                  text             Code for seating position of
                                             motor vehicle occupant: 1=
                                             driver, 2= center front, 3 =
                                             front passenger, 4 = second row
                                             left, 5 = second row center, 6 =
                                             second row right, 7 = enclosed
                                             passengers, 8 = exposed
                                             passengers, 9= unknown position,
                                             10 = third row left, 11 = third
                                             row center, 12 = third row right

  city                      text             City of residence of person
                                             involved in crash

  state                     text             State of residence of person
                                             involved in crash

  zipcode                   text             ZIP Code of residence of person
                                             involved in crash

  sex                       text             Gender of person involved in
                                             crash, as determined by
                                             reporting officer

  age                       number           Age of person involved in crash

  drivers\_license\_state   text             State issuing driver's license
                                             of person involved in crash

  drivers\_license\_class   text             Class of driver's license of
                                             person involved in crash

  safety\_equipment         text             Safety equipment used by vehicle
                                             occupant in crash, if any

  airbag\_deployed          text             Whether vehicle occupant airbag
                                             deployed as result of crash

  ejection                  text             Whether vehicle occupant was
                                             ejected or extricated from the
                                             vehicle as a result of crash

  injury\_classification    text             Severity of injury person
                                             sustained in the crash

  hospital                  text             Hospital to which person injured
                                             in the crash was taken

  ems\_agency               text             EMS agency who transported
                                             person injured in crash to the
                                             hospital

  ems\_run\_no              text             EMS agency run number

  driver\_action            text             Driver action that contributed
                                             to the crash, as determined by
                                             reporting officer

  driver\_vision            text             What, if any, objects obscured
                                             the driver's vision at time of
                                             crash

  physical\_condition       text             Driver's apparent physical
                                             condition at time of crash, as
                                             observed by the reporting
                                             officer

  pedpedal\_action          text             Action of pedestrian or cyclist
                                             at the time of crash

  pedpedal\_visibility      text             Visibility of pedestrian of
                                             cyclist safety equipment in use
                                             at time of crash

  pedpedal\_location        text             Location of pedestrian or
                                             cyclist at the time of crash

  bac\_result               text             Status of blood alcohol
                                             concentration testing for driver
                                             or other person involved in
                                             crash

  bac\_result\_value        number           Driver's blood alcohol
                                             concentration test result (fatal
                                             crashes may include pedestrian
                                             or cyclist results)

  cell\_phone\_use          text             Whether person was/was not using
                                             cellphone at the time of the
                                             crash, as determined by the
                                             reporting officer
  ---------------------------------------------------------------------------

Definitions
-----------

Examples
--------

```{=html}
<!--html_preserve-->
```
::: {#htmlwidget-ed911fa878d00703f645 .grViz .html-widget style="width:672px;height:480px;"}
:::

```{=html}
<script type="application/json" data-for="htmlwidget-ed911fa878d00703f645">{"x":{"diagram":"digraph g {\n  graph [rankdir = \"LR\"]\n  \n  node [fontsize = \"16\" shape=\"record\" fontname = \"Helvetica\"];\n  \"crash\" [label = \"Crash |{crash_record_id |<f0> abc123}| num_units | most_severe_injury | injuries_total | latitude | longitude\"];\n  \n  \"vehicle1\" [label = \"Vehicle 1 |<f0> crash_unit_id |{<f1> crash_record_id | abc123 }| {unit_no | 1} | unit_type | num_passengers | vehicle_use | DOT_no | ILCC_no\"];\n  \n  \"vehicle2\" [label = \"Vehicle 2 |<f0> crash_unit_id |{<f1> crash_record_id | abc123 }| {unit_no | 2} | unit_type | num_passengers | vehicle_use | DOT_no | ILCC_no\"];\n  \n  \"person1\" [label = \"Person 1 | person_id | person_type | crash_record_id |<f1> vehicle_id | seat_no | age | sex | airbag_deployed | injury_classification\"];\n  \n  \"person2\" [label = \"Person 2 | person_id | person_type |crash_record_id |<f1> vehicle_id | seat_no | age | sex | airbag_deployed | injury_classification\"];\n  \n  edge [arrowhead = \"none\"];\n  \"crash\":f0 -> \"vehicle1\":f1;\n  \"crash\":f0 -> \"vehicle2\":f1;\n  \"vehicle1\":f0 -> \"person1\":f1;\n  \"vehicle2\":f0 -> \"person2\":f1;\n}","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
```
```{=html}
<!--/html_preserve-->
```
Get the Code
------------

This project's source code is available [on
GitHub](https://github.com/mmmccarthy/chivz) and shared under the MIT
license.
