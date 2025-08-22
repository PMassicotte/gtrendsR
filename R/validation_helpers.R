#' Input validation helper functions for gtrends package
#'
#' @description
#' These functions provide consistent input validation with informative error
#' messages for the gtrends package.
#'
#' @param keyword Character vector of keywords
#' @return NULL (throws error if invalid)
#' @noRd
validate_keywords <- function(keyword) {
  if (!is.vector(keyword)) {
    stop(
      "The 'keyword' parameter must be a character vector.\n",
      "Example: keyword = c('NHL', 'NBA') or keyword = 'NHL'",
      call. = FALSE
    )
  }

  if (length(keyword) > 5L) {
    stop(
      "Maximum of 5 keywords allowed per query.\n",
      "You provided ",
      length(keyword),
      " keywords. ",
      "Consider splitting into multiple queries.",
      call. = FALSE
    )
  }

  if (anyNA(keyword) && length(keyword) > 1L) {
    stop(
      "Multiple keywords cannot include NA values.\n",
      "Use a single NA for category-only searches.",
      call. = FALSE
    )
  }
}

#' Validate geo parameters
#'
#' @param geo Character vector of geographic regions
#' @return NULL (throws error if invalid)
#' @noRd
validate_geo <- function(geo) {
  if (length(geo) > 5L) {
    stop(
      "Maximum of 5 geographic regions allowed per query.\n",
      "You provided ",
      length(geo),
      " regions. ",
      "Consider splitting into multiple queries.",
      call. = FALSE
    )
  }

  # Check geo format using regex
  if (all(nzchar(geo))) {
    m <- regexpr("^[a-zA-Z]{2}((?:-\\w{1,3}))?(?:-\\d{1,3})?", geo)
    ret <- regmatches(geo, m)

    if (!identical(ret, geo)) {
      invalid_geos <- geo[!geo %in% ret]
      stop(
        "Invalid geographic code(s): ",
        toString(invalid_geos),
        "\n",
        "Geographic codes must follow ISO format: 'US', 'US-CA', 'US-CA-807'.\n",
        "Use data('countries') to see valid country codes.",
        call. = FALSE
      )
    }
  }
}

#' Validate time parameter
#'
#' @param time Character string specifying time span
#' @return NULL (throws error if invalid)
#' @noRd
validate_time <- function(time) {
  if (length(time) > 5L) {
    stop(
      "Maximum of 5 time periods allowed per query.\n",
      "You provided ",
      length(time),
      " time periods.",
      call. = FALSE
    )
  }

  if (!check_time(time)) {
    stop(
      "Invalid time format: '",
      time,
      "'\n",
      "Valid formats include:\n",
      "  - Preset periods: 'now 1-H', 'today 1-m', 'today+5-y', 'all'\n",
      "  - Custom ranges: '2020-01-01 2020-12-31'\n",
      "  - Date-time ranges: '2020-01-01T01 2020-01-01T23'",
      call. = FALSE
    )
  }
}

#' Validate category parameter
#'
#' @param category Numeric category ID
#' @return NULL (throws error if invalid)
#' @noRd
validate_category <- function(category) {
  if (!all(category %in% categories[, "id"])) {
    invalid_cats <- category[!category %in% categories[, "id"]]
    stop(
      "Invalid category code(s): ",
      toString(invalid_cats),
      "\n",
      "Use data('categories') to see valid category codes.\n",
      "Example: category = 20 (Sports)",
      call. = FALSE
    )
  }
}

#' Validate language parameter
#'
#' @param hl Character string with language code
#' @return NULL (throws error if invalid)
#' @noRd
validate_language <- function(hl) {
  if (length(hl) != 1L || !is.character(hl)) {
    stop(
      "The 'hl' parameter must be a single character string.\n",
      "Example: hl = 'en-US' or hl = 'fr'",
      call. = FALSE
    )
  }

  if (!hl %in% language_codes$code) {
    stop(
      "Invalid language code: '",
      hl,
      "'\n",
      "Must be a valid ISO language code like 'en-US' or 'fr'.\n",
      "See available codes in the language_codes dataset.",
      call. = FALSE
    )
  }
}

#' Validate timezone parameter
#'
#' @param tz Numeric or character timezone specification
#' @return Numeric timezone offset in minutes
#' @noRd
validate_timezone <- function(tz) {
  if (is.numeric(tz)) {
    return(tz)
  }

  if (is.character(tz) && tz %in% OlsonNames()) {
    return(map_tz2min(tz))
  }

  stop(
    "Invalid timezone: '",
    tz,
    "'\n",
    "Must be either:\n",
    "  - Numeric offset in minutes (e.g., -120 for UTC-2)\n",
    "  - Valid timezone name (e.g., 'America/New_York')\n",
    "Use OlsonNames() to see valid timezone names.",
    call. = FALSE
  )
}

#' Validate compared breakdown parameters
#'
#' @param compared_breakdown Logical flag
#' @param geo Character vector of geographic regions
#' @param keyword Character vector of keywords
#' @return NULL (throws error if invalid)
#' @noRd
validate_compared_breakdown <- function(compared_breakdown, geo, keyword) {
  if (!is.logical(compared_breakdown)) {
    stop(
      "The 'compared_breakdown' parameter must be TRUE or FALSE.",
      call. = FALSE
    )
  }

  if (compared_breakdown && (length(geo) != 1L || length(keyword) == 1L)) {
    stop(
      "Compared breakdown requires exactly one geographic region and multiple keywords.\n",
      "You provided ",
      length(geo),
      " geo region(s) and ",
      length(keyword),
      " keyword(s).\n",
      "Example: gtrends(c('NHL', 'NBA'), geo = 'US', compared_breakdown = TRUE)",
      call. = FALSE
    )
  }
}

#' Validate parameter combinations
#'
#' @param keyword Character vector of keywords
#' @param geo Character vector of geographic regions
#' @param time Character vector of time periods
#' @return NULL (throws error if invalid)
#' @noRd
validate_parameter_combinations <- function(keyword, geo, time) {
  # Check vector length compatibility
  valid_combination <- (length(keyword) %% length(geo) == 0L) ||
    (length(geo) %% length(keyword) == 0L) ||
    (length(time) %% length(keyword) == 0L)

  if (!valid_combination) {
    stop(
      "Parameter lengths are incompatible.\n",
      "Keywords: ",
      length(keyword),
      ", Geo: ",
      length(geo),
      ", Time: ",
      length(time),
      "\n",
      "One vector should be a multiple of the others for proper combinations.",
      call. = FALSE
    )
  }
}
