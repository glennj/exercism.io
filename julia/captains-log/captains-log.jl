using Random

planetary_classes = collect("DHJKLMNRTY")

random_planet() = rand(planetary_classes)

random_ship_registry_number() = "NCC-$(rand(1000:9999))"

random_stardate() = 41000 + 1000 * rand()

random_stardate_v2() = rand(41000:41999) + rand(0.0:0.1:0.9)

pick_starships(starships, number_needed) = Random.shuffle(starships)[1:number_needed]
