get_diff <- function(table = "penguins") {
  # Compose file path and expected commit message pattern
  oddjob_path <- paste0(table, "/pipe_", table, ".yml")
  commit_message <- paste0("spectre a/m pipe_", table, ".yml")

  # Setup GitLab connection safely and with feedback
  glcon <- tryCatch(
    {
      gitlabr::set_gitlab_connection(
        gitlab_url = "https://gitlab.lrz.de",
        private_token = Sys.getenv("GITLAB_API_TOKEN")
      )
    },
    warning = function(w) {
      rlang::warn(message = w$message, class = "gitlab_warning")
      NULL
    },
    error = function(e) {
      rlang::abort(message = e$message, class = "gitlab_error")
    }
  )

  if (rlang::is_null(glcon)) {
    cli::cli_abort("Cannot connect to GitLab. Check your token and URL.")
  }

  # Fetch commits filtering on file path and limiting to last 100 commits for performance
  project_commits <- gitlabr::gl_get_commits(
    project = "216273",
    path = oddjob_path,
    per_page = 100
  )

  # Check if any commits were returned
  if (nrow(project_commits) == 0) {
    cli::cli_warn("No commits found for file {.file {oddjob_path}}")
    gitlabr::unset_gitlab_connection()
    return(character(0))
  }

  # Convert commit dates to datetime for sorting
  project_commits$authored_date2 <- lubridate::ymd_hms(project_commits$authored_date, tz = "UTC")

  # Filter commits matching the exact commit message
  filtered_commits <- project_commits |>
    dplyr::filter(message == commit_message) |>
    dplyr::arrange(dplyr::desc(authored_date2))

  if (nrow(filtered_commits) == 0) {
    cli::cli_warn("No commits found with message {.val {commit_message}}")
    gitlabr::unset_gitlab_connection()
    return(character(0))
  }

  latest_commit_id <- filtered_commits$id[1]

  # Get diff for the latest commit
  diff_project <- gitlabr::gl_get_diff(project = "216273", commit_sha = latest_commit_id)

  # Filter the diff for the exact file path
  onefile <- diff_project |>
    dplyr::filter(new_path == oddjob_path | old_path == oddjob_path)

  if (nrow(onefile) == 0) {
    cli::cli_warn("No diff found for file {.file {oddjob_path}} in commit {.val {latest_commit_id}}")
    gitlabr::unset_gitlab_connection()
    return(character(0))
  }

  diff_text <- onefile$diff[[1]]

  # Split the diff text into lines
  diff_lines <- strsplit(diff_text, "\n")[[1]]

  # Clean up connection on exit
  on.exit(gitlabr::unset_gitlab_connection(), add = TRUE)

  return(diff_lines)
}



visualize_diff <- function(diff, browse = TRUE) {

  # Build HTML lines for diff with classes and line numbers
  html_diff_lines <- character()

  # Initialize line counters for old and new files
  old_line_num <- 0
  new_line_num <- 0

  for (line in diff) {
    # Chunk header lines start with @@ -old_start,old_count +new_start,new_count @@
    if (grepl("^@@", line)) {
      # Extract starting line numbers from chunk header
      matches <- regmatches(line, regexec("^@@ -(\\d+),\\d+ \\+(\\d+),\\d+ @@", line))
      if (length(matches[[1]]) >= 3) {
        old_line_num <- as.integer(matches[[1]][2]) - 1
        new_line_num <- as.integer(matches[[1]][3]) - 1
      }
      # Style chunk header lines differently
      html_diff_lines <- c(html_diff_lines,
                           sprintf("<div class='diff-chunk-header' style='color: #999; font-style: italic;'>%s</div>", line))
      next
    }

    # Check line type: added, removed, or unchanged
    first_char <- substr(line, 1, 1)

    # Increment line numbers accordingly
    if (first_char == "+") {
      new_line_num <- new_line_num + 1
      old_num_display <- "&nbsp;&nbsp;&nbsp;"
      new_num_display <- sprintf("%4d", new_line_num)
      line_content <- substr(line, 2, nchar(line))
      line_html <- sprintf("<span class='line-num old-line'>&nbsp;</span><span class='line-num new-line'>%s</span> <span class='added'>+ %s</span>",
                           new_num_display, htmltools::htmlEscape(line_content))

    } else if (first_char == "-") {
      old_line_num <- old_line_num + 1
      old_num_display <- sprintf("%4d", old_line_num)
      new_num_display <- "&nbsp;&nbsp;&nbsp;"
      line_content <- substr(line, 2, nchar(line))
      line_html <- sprintf("<span class='line-num old-line'>%s</span><span class='line-num new-line'>&nbsp;</span> <span class='removed' style='text-decoration: line-through;'>- %s</span>",
                           old_num_display, htmltools::htmlEscape(line_content))

    } else {
      # Context / unchanged line
      old_line_num <- old_line_num + 1
      new_line_num <- new_line_num + 1
      old_num_display <- sprintf("%4d", old_line_num)
      new_num_display <- sprintf("%4d", new_line_num)
      line_content <- line
      line_html <- sprintf("<span class='line-num old-line'>%s</span><span class='line-num new-line'>%s</span> <span class='context'>  %s</span>",
                           old_num_display, new_num_display, htmltools::htmlEscape(line_content))
    }

    html_diff_lines <- c(html_diff_lines, line_html)
  }

  # Wrap in <pre><code> for formatting
  final_html <- paste0(
    "<pre><code>",
    paste(html_diff_lines, collapse = "\n"),
    "</code></pre>"
  )

  # CSS styling for diff visualization
  custom_css <- "
<style>
  pre {
    background-color: #f7f7f7;
    padding: 10px;
    border-radius: 5px;
    border: 1px solid #ccc;
    overflow-x: auto;
    font-family: 'Courier New', monospace;
    font-size: 14px;
  }
  .line-num {
    display: inline-block;
    width: 3em;
    text-align: right;
    padding-right: 1em;
    color: #999;
    user-select: none;
  }
  .old-line {
    color: #999;
  }
  .new-line {
    color: #999;
  }
  .added {
    color: green;
  }
  .removed {
    color: red;
    text-decoration: line-through;
  }
  .context {
    color: #444;
  }
  .diff-chunk-header {
    margin-top: 1em;
  }
</style>"

  html_full <- paste0(custom_css, final_html)

  if (browse) {
    htmltools::browsable(htmltools::HTML(html_full))
  } else {
    return(htmltools::HTML(html_full))
  }
}


#diff_lines <- get_diff()
#visualize_diff(diff = diff_lines, browse = TRUE)









