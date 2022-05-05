# An empty config
empty_config = list(
    "xml" = list(),
    "templates" = list(),
    "defaults" = list()
    )

# A simple config
config = list(
    "xml" = list(
        "chunk_one" = "This is chunk {{one}}",
        "chunk_two" = "This is chunk {{two}}"
        ),
    "templates" = list(),
    "defaults" = list(
        "one" = 1,
        "two" = 2
        )
    )

# Processed config
processed = list(
    "chunk_one" = "This is chunk 1",
    "chunk_two" = "This is chunk 2"
    )
