# boxview() snapshots

    Code
      boxview(ave)
    Output
      function (x, ..., FUN = mean) 
      ┌───────────────────────────────────────────────────────────────┐
      | if (missing(...))                                             |
      | ┌───────────────┐ ┌─────────────────────────────────────────┐ |
      | | x[] <- FUN(x) | | g <- interaction(...)                   | |
      | └───────────────┘ | split(x, g) <- lapply(split(x, g), FUN) | |
      |                   └─────────────────────────────────────────┘ |
      | x                                                             |
      └───────────────────────────────────────────────────────────────┘

---

    Code
      boxview(ave, width = 40)
    Output
      function (x, ..., FUN = mean) 
      ┌───────────────────────────────┐
      | if (missing(...))             |
      | ┌────────┐ ┌────────────────┐ |
      | | x[] <- | | g <-           | |
      | |   FUN( | |   interaction( | |
      | |     x  | |     ...        | |
      | |   )    | |   )            | |
      | └────────┘ | split(         | |
      |            |   x, g         | |
      |            | ) <-           | |
      |            |   lapply(      | |
      |            |     split(     | |
      |            |       x,       | |
      |            |       g        | |
      |            |     ),         | |
      |            |     FUN        | |
      |            |   )            | |
      |            └────────────────┘ |
      | x                             |
      └───────────────────────────────┘

---

    Code
      boxview(ave, width = 40, optimization = "strong")
    Output
      function (x, ..., FUN = mean) 
      ┌───────────────────────────────┐
      | if (missing(...))             |
      | ┌────────┐ ┌────────────────┐ |
      | | x[] <- | | g <-           | |
      | |   FUN( | |   interaction( | |
      | |     x  | |     ...        | |
      | |   )    | |   )            | |
      | └────────┘ | split(x, g) <- | |
      |            |   lapply(      | |
      |            |     split(     | |
      |            |       x,       | |
      |            |       g        | |
      |            |     ), FUN     | |
      |            |   )            | |
      |            └────────────────┘ |
      | x                             |
      └───────────────────────────────┘

# swapcall() snapshots

    Code
      swap_calls(quote({
        a <- if (this) {
          if (this) a else b
        } else b
      }))
    Output
      {
          if (this) {
              if (this) 
                  a <- a
              else a <- b
          }
          else a <- b
      }

