#### get phylopic stuff ####
install.packages("rphylopic")
install.packages("magrittr")
install.packages("ggimage")
library(here)
library(rphylopic)
library(magrittr)
library(ggimage)
uuid1 <- get_uuid(name = "Anguilla rostrata")
uuid2 <- get_uuid(name = "Sinipercidae")
uuid3 <- get_uuid(name = "Lutjanus griseus")
uuid4 <- get_uuid(name = "Boreogadus saida")
uuid5 <- get_uuid(name = "fundulus heteroclitus macrolepidotus")
uuid6 <- get_uuid(name = "Salmo salar")
uuid7 <- get_uuid(name = "Gadus chalcogrammus")


img1 <- get_phylopic(uuid = uuid1)
img2 <- get_phylopic(uuid = uuid2)
img3 <- get_phylopic(uuid = uuid3)
img4 <- get_phylopic(uuid = uuid4)
img5 <- get_phylopic(uuid = uuid5)
img6 <- get_phylopic(uuid = uuid6)
img7 <- get_phylopic(uuid = uuid7)

uuid7 <- plot(x = 1, y = 1, type = "n", ann = FALSE) %>%
  add_phylopic_base(img = img7, x = 1, y = 1, height = 0.25)
uuid7<- ggplot() +
  coord_cartesian(xlim = c(0, 2), ylim = c(0.6, 1.8)) +
  add_phylopic(img = img7, x = 1, y = 1, height = 0.25)

ggsave(filename = here("figures", "fig2aphylopic.pdf"), plot = uuid7, width = 6, height = 4.5)



freshwater_phylos <- ggplot() +
  coord_cartesian(xlim = c(0.6, 1.4), ylim = c(0.6, 1.4)) +
  add_phylopic(img = img1, x = 1.25, y = 1.25, height = 0.25) +
  add_phylopic(img = img2, x = 1, y = 1, height = 0.25) +
  add_phylopic(img = img6, x = 0.9, y = 0.75, height = 0.25,
               fill = "original")

ggsave(filename = here("figures", "freshwater_phylopics.pdf"), plot = freshwater_phylos, width = 6, height = 4.5)

marine_phylos <- ggplot() +
  coord_cartesian(xlim = c(0.6, 1.4), ylim = c(0.6, 1.4)) +
  add_phylopic(img = img3, x = 1, y = 1.25, height = 0.25) +
  add_phylopic(img = img5, x = 1, y = 1, height = 0.25) +
  add_phylopic(img = img4, x = 0.9, y = 0.75, height = 0.25,
               fill = "original")
marine_phylos
ggsave(filename = here("figures", "marine_phylopics.pdf"), plot = marine_phylos, width = 6, height = 4.5)
