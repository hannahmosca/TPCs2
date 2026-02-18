####this is to make a big supplemental table that reports the models that were used to fit each curve, along with the estimated parameter values when present ####
library(here)
library(tidyr)
rm(list=ls())
model_parameters <- readRDS(here("processed-data", "sorted_datasets_withparams.RDS"))

## add a column based on a list of Ea curves i made in script 14 (Ea_curves)

# (Ea_curves)
# [1] 211 352 379 380  23  71  81  75  74  65 132 146 153 152 163 195 244 269 265 266 268 293
# [23] 301 305 314 317 295 297 302 337 331 338 335 343 342 416 432 452 453 455 234 300 362 367
# [45] 366 375 396 395 398 391 402  17  59  58  70 160 193 202 206 238 242 247 250 251 255 299
# [67] 298 318 424 425 427 434 435 442 443 447 454 385 389 388 397 392 401 404 403 407 406  16
# [89]  18  51 148 154 165 172 171 213 215 316 423 433 448 446 370  20  76  67  80  73 307 315
# [111] 313 296 303 350 200 245 289 365 382 390 393   7  15  22  52 151 149 203 204 212 249 248
# [133] 308 309 310 306 294 336 341 440 441 450 256 267 387  83  78  66 131 194 241 246 325 349
# [155] 449  64 278 359  21 133 150 205 210 216 218 444 222 409  61  62 147 187 259 260 274 291
# [177] 286 287 288 290 285 281 282 283 284 280 275 276 277 279 351 418 420 445   1 333 372 405
# [199] 383   2  13 394  24  35  46  57  68  79 101 112 123 134 145 156  90 167 371 408   6   5
# [221] 415 414 413 412 411 410  41  42  43  82  95  94  93  85 105 104  30  60 102  97  98  99
# [243] 100 110 137 138 141 142  25  26 224 227 229 230 233 237 214 257 258 270 271 272 273 312
# [265] 329 330 339 347 348 346 421 419 430 459 457

model_parameters <- model_parameters %>%
  mutate(Ea_TF = ifelse(curve_ID %in% Ea_curves, TRUE, FALSE))

## updating paramater values based on TF columns (for export/sup)##
#first, adjust some names
model_parameters_renamed <- model_parameters %>%
  rename(thermal_min = ctmin) %>%
  rename(thermal_max = ctmax) %>%
  rename(performance_breadth = breadth) %>%
  rename(tolerance_breadth = thermal_tolerance) %>%
  rename(thermal_optimum = topt) %>%
  rename(activation_energy = e)

model_parameters_renamed <- model_parameters_renamed %>%
  mutate(thermal_min = ifelse(thermal_min_TF == FALSE, NA,
                              thermal_min),
         thermal_max = ifelse(thermal_max_TF == FALSE, NA,
                              thermal_max),
         thermal_optimum = ifelse(topt_TF == FALSE, NA,
                                  thermal_optimum),
         performance_breadth = ifelse(breadth_TF == FALSE, NA,
                                      performance_breadth),
         tolerance_breadth = ifelse(thermal_tolerance_TF == FALSE, NA,
                                    tolerance_breadth),
         activation_energy = ifelse(Ea_TF == FALSE, NA, 
                                    activation_energy)
         )


## for dataset type = irregular, i want NA in everything
## for dataset type = topt, i want topt TF,if topt
## need to find the list of curves that i said have valid AE (maybe this ends up being the last thing i do--so its like ok now for all curves where EA_TF == FALSE, put NA in EA, but make everythign else keep default value)
