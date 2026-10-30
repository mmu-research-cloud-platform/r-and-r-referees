# Data Dictionary: `data.syn.csv`

This file describes the variables contained within the synthetic dataset used for the reflection project. The data fully simulates physical match performances of soccer referees and players originally published at <https://www.tandfonline.com/doi/abs/10.1080/02640414.2010.543914>.

| Column Name | Data Type | Units / Format | Description |
|:-----------------|:-----------------|:-----------------|:-----------------|
| `Match` | Integer | Unique ID (1 to 236) | Identifier for each individual match. |
| `id` | Integer | Unique ID (1 to 18) | Identifier with two levels (referee, player [player data were the mean outfield player distance on each referee's match]) |
| `Role` | Character | Player or Referee | Role during the match. |
| `Time` | Integer | 5 to 90, by 5 | Specific 5-min time epochs across the first half (0 to 45 min) and second half (45 to 90 min). Data for stoppage time at the end of each half were not provided by the data supplier. |
| `Variable` | Character | Low-Speed or High-Speed | Low or high-speed activity variable. |
| `Distance` | Numeric | Meters (m) | The total distance during a 5-min time epoch. |
| `subject_id` | Character | `id.Role` (e.g., `ID_07.Player`) | Combined referee/player identifier used as the random-effect grouping for each subject. |
| `Half` | Character | First or Second | Match half. `First` when `Time` is 5 to 45 min; `Second` when `Time` is 50 to 90 min. |
| `Time_half` | Integer | 5 to 45, by 5 | Time within the half. Equals `Time` in the first half and `Time - 45` in the second half. |
| `H1` | Integer (0/1) | 1 = first half | Indicator for the first half. |
| `H2` | Integer (0/1) | 1 = second half | Indicator for the second half. |
| `RefH1` | Integer (0/1) | 1 = referee, first half | Indicator for referee rows in the first half. |
| `RefH2` | Integer (0/1) | 1 = referee, second half | Indicator for referee rows in the second half. |
| `PlayerH1` | Integer (0/1) | 1 = player, first half | Indicator for player rows in the first half. |
| `PlayerH2` | Integer (0/1) | 1 = player, second half | Indicator for player rows in the second half. |
| `replicate` | Integer | 1 to 10 | Synthetic replicate number. Present only in the long-format file with all replicates; the public single-replicate file omits it. |

------------------------------------------------------------------------

## Dataset Notes

- **Data Nature:** This is a fully synthetic dataset (`data.syn.csv`) that mirrors the statistical distributions and hierarchical structure (e.g., matches nested within referees) of the original data.
- **Anonymisation:** All match, player, and referee identifiers have been completely anonymised using randomized integer sequences.
