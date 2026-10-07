# Activity logging for process mining ---------------------------------------------
#
# Raw Shiny events (input changes, navigation) are captured client-side by
# www/activity.js, batched and sent to the server every few seconds. Each raw
# input id is mapped to a human-readable process phase via the
# `activity_phases` config. The resulting event log (case, activity,
# timestamp, resource) is the standard input for process-mining tools
# (bupaR, pm4py, Celonis).

#' Map raw input ids to process phases
#' @param input_id Character vector of Shiny input ids (module-namespaced).
#' @param cfg Config list.
#' @return Character vector of phases (same length).
#' @export
wt_map_activity_phase <- function(input_id, cfg) {
  phases <- rep("Other", length(input_id))
  done <- rep(FALSE, length(input_id))
  for (p in cfg$activity_phases) {
    hit <- !done & grepl(p$pattern, input_id)
    phases[hit] <- p$phase
    done <- done | hit
  }
  phases
}

#' Collapse a raw activity log into a process-mining event log
#'
#' Consecutive raw events of the same session and phase become one activity
#' instance with start/end timestamps.
#' @param log data.frame(ts, session_id, user, phase, opp_id).
#' @return data.frame(case_id, session_id, activity, resource, start, end, n_events).
#' @export
wt_activity_eventlog <- function(log) {
  if (is.null(log) || !nrow(log)) {
    return(data.frame(case_id = character(), session_id = character(), activity = character(),
                      resource = character(), start = as.POSIXct(character()), end = as.POSIXct(character()),
                      n_events = integer()))
  }
  log <- log[order(log$session_id, log$ts), ]
  new_block <- c(TRUE, log$session_id[-1] != log$session_id[-nrow(log)] | log$phase[-1] != log$phase[-nrow(log)])
  block <- cumsum(new_block)
  out <- do.call(rbind, lapply(split(seq_len(nrow(log)), block), function(ix) {
    data.frame(case_id = log$opp_id[ix[length(ix)]], session_id = log$session_id[ix[1]],
               activity = log$phase[ix[1]], resource = log$user[ix[1]],
               start = min(log$ts[ix]), end = max(log$ts[ix]), n_events = length(ix),
               stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  out
}

#' Directly-follows counts between phases (process map edges)
#' @param eventlog Output of [wt_activity_eventlog()].
#' @return data.frame(from, to, n).
#' @export
wt_phase_transitions <- function(eventlog) {
  if (nrow(eventlog) < 2) return(data.frame(from = character(), to = character(), n = integer()))
  el <- eventlog[order(eventlog$session_id, eventlog$start), ]
  same <- el$session_id[-1] == el$session_id[-nrow(el)]
  edges <- data.frame(from = el$activity[-nrow(el)][same], to = el$activity[-1][same], stringsAsFactors = FALSE)
  if (!nrow(edges)) return(data.frame(from = character(), to = character(), n = integer()))
  agg <- stats::aggregate(list(n = rep(1L, nrow(edges))), by = edges, FUN = sum)
  agg[order(-agg$n), ]
}
