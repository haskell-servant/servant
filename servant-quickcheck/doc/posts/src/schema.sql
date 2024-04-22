CREATE TABLE genus (
    genus_name     text  PRIMARY KEY,
    genus_family   text  NOT NULL
);

CREATE TABLE species (
    species_name    text  PRIMARY KEY,
    species_genus   text  NOT NULL REFERENCES genus (genus_name)
)
