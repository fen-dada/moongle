# Notes

## Mooncakes stuff
moon downloads packages using the following url:
```text
https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user/<username>/<modulename>/<version>.zip
```

we can get all available packages from:
```text
https://mooncakes.io/assets/modules.json
```

the content looks like this:
```json
  "modules": [
    {
      "name": "rami3l/cmark",
      "version": "0.3.0",
      "deps": {
        "myfreess/casefold": "0.1.1",
        "myfreess/charclass": "0.1.1"
      },
      "readme": "README.md",
      "repository": "https://github.com/moonbit-community/cmark",
      "license": "Apache-2.0",
      "keywords": [
        "commonmark",
        "markdown"
      ],
      "description": "A CommonMark toolkit for MoonBit",
      "source": "src",
      "checksum": "ea4e690cc46c2aa6e36560d624ea9255448b9ca0a99aa802d4e67568206cec7c",
      "created_at": "2025-05-30T07:50:37.620275+00:00",
      "is_first_version": false
    },
  ]
```
