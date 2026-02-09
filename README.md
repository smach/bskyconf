# bskyconf

Monitor Bluesky conference posts in an interactive Shiny app.

bskyconf fetches posts matching conference hashtags from the Bluesky API, deduplicates them, filters out bot and protected accounts, and serves them in a searchable Shiny app with metric cards, date/author filters, and CSV export. 

The original project was written completely by me for the Twitter API. I updated it as plain R scripts with the help of Posit's Shiny Assistant to use the Bluesky API. This version as an R package was re-written largely by Posit Assistant + Claude Opus 4.6 with guidance and supervision by me.

Most of the following documentation was writen by Claude, lightly edited by me.

## Installation

```r
# Install from GitHub
pak::pak("smach/bskyconf", dependencies = TRUE)

# Or with remotes
remotes::install_github("smach/bskyconf", build_vignettes = TRUE)
```

## Quick Start

### 1. Set up Bluesky credentials

Create an [app password](https://bsky.app/settings/app-passwords) on Bluesky, then add your credentials to your `.Renviron` file:
```
BLUESKY_APP_USER=yourhandle.bsky.social
BLUESKY_APP_PASS=your-app-password
```

Restart R after editing `.Renviron` for changes to take effect.

### 2. Create a conference directory

Example for the NICAR 2026 conference:

```r
library(bskyconf)

setup_conference(
  path = "~/my_conferences/nicar2026",
  conference_name = "NICAR 2026",
  hashtags = c("#NICAR26", "#NICAR2026"),
  min_date = as.Date("2026-01-01"),
  exclude_accounts_regex = "\\.ap\\.brid\\.gy"
)
```

This creates a ready-to-deploy directory with:
- `_bskyconf.yml` -- all conference settings
- `app.R` -- Shiny Server entry point
- `update_posts.R` -- cron job script
- `data/` -- empty data directory

### 3. Fetch posts and launch the app

```r
# Fetch posts (run this manually or via cron)
update_conference_posts(
  hashtags = c("#NICAR26", "#NICAR2026"),
  data_dir = "~/my_conferences/nicar2026/data",
  min_date = as.Date("2026-01-01")
)

# Launch the app locally
run_conference_app(
  conference_name = "NICAR 2026",
  data_file = "~/my_conferences/nicar2026/data/table_posts.Rds"
)
```

## Configuration

All settings live in `_bskyconf.yml`:

| Field | Description |
|-------|-------------|
| `conference_name` | Display name (e.g. "NICAR 2026") |
| `conference_slug` | Short name for filenames (e.g. "nicar2026") |
| `hashtags` | List of hashtags to track |
| `data_dir` | Relative path for data files |
| `posts_per_hashtag` | Number of posts to fetch per hashtag (default: 250) |
| `min_date` | Earliest date for posts (filters out prior years) |
| `exclude_accounts` | List of account handles to always exclude |
| `exclude_accounts_regex` | Regex pattern to exclude accounts (e.g., bridge accounts) |
| `about_text` | Description shown in the FAQ modal |
| `update_text` | Update frequency description in the FAQ modal |

## Automated Updates with Cron

Set up a cron job to fetch new posts periodically:

```
# Every 30 minutes during the conference
*/30 * * * * /usr/bin/Rscript /srv/shiny-server/nicar2026/update_posts.R >> /var/log/bskyconf/nicar2026.log 2>&1
```

The generated `update_posts.R` script reads the config file and calls `update_conference_posts()` automatically.

## Shiny Server Deployment

1. Run `setup_conference()` in your Shiny Server apps directory (e.g. `/srv/shiny-server/nicar2026`)
2. Run the update script once to fetch initial data
3. Set up the cron job for ongoing updates
4. The app is immediately available at `http://your-server/nicar2026`

There are other deployment options, such as [Posit Connect Cloud](https://connect.posit.cloud/).

## Key Functions

| Function | Purpose |
|----------|---------|
| `setup_conference()` | Scaffold a new conference directory |
| `update_conference_posts()` | Fetch, deduplicate, and save posts (cron entry point) |
| `run_conference_app()` | Launch the Shiny app |
| `fetch_hashtag_posts()` | Fetch posts for a single hashtag |
| `bs_conference_auth()` | Authenticate with Bluesky |
| `read_conference_config()` | Read a `_bskyconf.yml` config file |

## Example

See a live example at [apps.machlis.com/shiny/positconf2025](https://apps.machlis.com/shiny/positconf2025/).

## License

MIT
