get_winviz <- function(x) {
  
  if (length(x$content$innings)==0) { return(NULL) }
  
  match_data <- data.frame(
    series_id = as.numeric(x$match$series$id),
    series = x$match$series$name,
    season_id = as.numeric(x$match$series$objectId),
    season = x$match$series$season,
    match_id = x$match$objectId,
    date_time = x$match$startTime,
    venue = x$match$ground$longName,
    home_team_id = x$match$teams[[1]]$team$id,
    home_team = x$match$teams[[1]]$team$longName,
    away_team_id = x$match$teams[[2]]$team$id,
    away_team = x$match$teams[[2]]$team$longName,
    toss_winner = ifelse(is.null(x$match$tossWinnerTeamId), NA, x$match$tossWinnerTeamId),
    toss_choice = ifelse(is.null(x$match$tossWinnerChoice), NA, x$match$tossWinnerChoice),
    match_winner_id = ifelse(is.null(x$match$winnerTeamId), NA, x$match$winnerTeamId))
  
  innings_data <- purrr::map_df(
    c(1:length(x$content$innings)), ~ {
      return(data.frame(
        innings = .x,
        batting_team = as.character(x$content$innings[[.x]]$team$longName),
        over_number = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) as.numeric(y$overNumber))),
        bowler_id = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$bowlers[[1]]$id)),
        bowler = as.character(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$bowlers[[1]]$longName)),
        over_runs = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$overRuns)),
        over_wickets = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$overWickets)),
        total_runs = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$totalRuns)),
        total_wickets = as.numeric(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$totalWickets)),
        current_run_rate = as.numeric(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$overRunRate)),
        required_run_rate = as.numeric(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$requiredRunRate)),
        required_runs = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$requiredRuns)),
        remaining_balls = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) y$remainingBalls)),
        proj_score = as.integer(sapply(x$content$innings[[.x]]$inningOvers, function(y) ifelse(is.null(y$predictions), NA, y$predictions$score))),
        win_prob = as.numeric(sapply(x$content$innings[[.x]]$inningOvers, function(y) ifelse(is.null(y$predictions), NA, y$predictions$winProbability))))
      )
    }
  )
  
  return(
    cbind(match_data |> dplyr::slice(rep(1:n(), each = nrow(innings_data))),innings_data)
  )
}

matches <- read.csv("https://raw.githubusercontent.com/jaidenpopowski/WinViz/main/match_ids.csv")

pb <- suppressWarnings(dplyr::progress_estimated(length(matches$match_id)))

winviz <- purrr::pmap_df(list(matches$tid,matches$match_id), ~ {
  pb$tick()$print() # update progress bar
  get_winviz(httr::content(httr::GET(url = paste0('https://hs-consumer-api.espncricinfo.com/v1/pages/match/scorecard?lang=en&seriesId=',{..1},'&matchId=',{..2}))))
}) |> 
  dplyr::mutate(
    win_prob_home = ifelse(home_team==batting_team, win_prob_batting, 100-win_prob_batting),
    match_winner = ifelse(match_winner_id==home_team_id, home_team, away_team),
    toss_winner = ifelse(toss_winner == home_team_id, home_team, away_team),
    toss_choice = ifelse(toss_choice == 1, "Will Bat","Will Bowl")) |> 
  dplyr::relocate(match_winner, .after = match_winner_id) |> 
  dplyr::mutate(required_runs = ifelse(innings == 1, NA, required_runs), required_run_rate = ifelse(innings == 1, NA, required_run_rate), remaining_balls = ifelse(innings == 1, NA, remaining_balls)) 
