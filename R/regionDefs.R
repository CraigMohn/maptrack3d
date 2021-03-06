expandRegions <- function(invec,country) {
  
  ##  sub in vec of states for matching region in apprpriate country
  regionlist <- c("PacificNorthWest",
                  "MountainWest",
                  "DelMarVa",
                  "NewEngland",
                  "Maritimes")
  regionstates <- list(c("WA","OR","ID","MT"),
                       c("WA","OR","ID","MT","WY","CO","UT","NV"),
                       c("DE","MD","DC","VA"),
                       c("ME","NH","VT","MA","RI","CT"),
                       c("NS","PE","NB","NF")
  )
  regioncountry <- c("US","US","US","US","CANADA")
  
  outvec <- invec
  for (i in 1:length(regionlist)) {
    if ((toupper(regionlist[[i]]) %in% toupper(outvec)) &
        (toupper(regioncountry[[i]]) == toupper(country))) {
      outvec <- setdiff(outvec,toupper(regionlist[[i]]))
      outvec <- union(outvec,toupper(regionstates[[i]]))
    }
  }
  return(outvec)
}  

# cropbox <- raster::extent(-160.25, -154.8, -18.9, 22.25) # hawaii main islands only
# cropbox <- raster::extent(-172,-160.25, -18.9, 40) # hawaii NW only


# mapWindow <- c(-159.90,-159.15,21.75,22.35) # Kauai -purple
# mapWindow <- c(-156.80,-155.97,20.50,21.05) # Maui - magenta
# mapWindow <- c(-156.10,-154.75,18.85,20.30) # Big Island - red
# mapWindow <- c(-158.30,-157.60,21.20,21.75) # Oahu - gold

# mapWindow <- c(-123.25,-121.5,46.75,48.1)   # Seattle Area 
# mapWindow <- c(-122.2,-121.7,47.4,47.8)     # Samm Area 
# mapWindow <- c(-122.4,-122.1,47.5,47.8)     # Lake Wash Area 
# mapWindow <- c(-123.2,-122.4,48.3,48.8)     # San Juans

# mapWindow <- c(-122.5,-121.9,37.6,38.1)     # East Bay 
# mapWindow <- c(-123.1,-120.9,36.4,38.4)     # SF Bay 
# mapWindow <- c(-122.45,-121.85,37.05,37.65) # Penninsula and South Bay 
# mapWindow <- c(-123.1,-122.42,37.81,38.25)  # Marin County 
# mapWindow <- c(-122.45,-121,40.3,41.6)      # CA Volcanos 

# mapWindow <- c(-81.0,-80.0,36.9,37.6)       # Giles Cty/Blacksburg Area 
# mapWindow <- c(-62.0,-59.5,45.4,47.11)      # Cape Breton Island 

# mapWindow <- c(144.25,148.8,-43.75,-39.4)   # Tasmania 
# mapWindow <- c(145.75,145.95,-41.7,-41.5)   # Cradle Mountain 
# mapWindow <- c(130.6,131.1,-25.4,-25.2)     # Uluru and The Olgas 

