#' Play or save musical chords
#'
#' @name rechordr
#'
#' @usage rechordr(pitch, duration, tempo = 120, sample_rate = 44100, fade_duration = 0.02, output_path = NULL)
#'
#' @param pitch a character vector or list of notes using the Latin alphabet. Use a list to play multiple notes simultaneously, placing all notes to be played together in a character vector. An element with no characters results in silence. Append:
#' \itemize{
#'   \item `#` for sharp semitones
#'   \item `b` for flat semitones
#'   \item octave number, by default octave 4
#'   }
#' @param duration a numeric vector of each note's duration, in beats. Must be same length as pitch.
#' @param tempo integer, tempo to be played in beats per minute.
#' @param sample_rate integer, number of samples per second (Hz).
#' @param fade_duration number of seconds to fade each note in and out. Can be used to avoid audible pops at the beginning and end of each note. Cannot exceed half of the shortest note's duration.
#' @param output_path path to location where wave file will be written. If `NULL`, music is played in the console.
#'
#' @return Music played in R console, or saved wave file.
#' @author Ariel Fridman
#'
#' @examples
#' rechordr(pitch = list(c("A3", "C4", "F4"), c("G3", "C4", "E4"), c("G3", "C4", "E4"), c("F3", "Bb3", "D4"), "", c("A3", "C4", "F4"), c("G3", "C4", "E4"), c("G3", "C4", "E4"), c("A3", "C4", "F4")),
#'          duration = c(1, .75, .75, .5, 1, 1, .75, .75, .5))
#'
#' rechordr(pitch = list(c("A3", "C4", "F4"), c("G3", "C4", "E4"), c("G3", "C4", "E4"), c("F3", "Bb3", "D4"), "", c("A3", "C4", "F4"), c("G3", "C4", "E4"), c("G3", "C4", "E4"), c("A3", "C4", "F4")),
#'          duration = c(1, .75, .75, .5, 1, 1, .75, .75, .5),
#'          output_path = "You Can Call Me Al.wav")
#'
#' @export

rechordr <- function(pitch, duration, tempo = 120, sample_rate = 44100, fade_duration = 0.02, output_path = NULL) {
  
  tune <- data.frame(
    pitch = I(pitch),
    duration = duration)
  tune$id <- 1:nrow(tune)
  
  max_duration <- (min(tune$duration)/tempo)*60/2
  if (fade_duration>max_duration) {
    fade_duration <- max_duration
    warning(paste0(c("Fade duration changed to ", max_duration, ". Cannot exceed half of the shortest note's duration.")))
  }
  
  rep_times <- vapply(tune$pitch, length, 1L)
  tune <- data.frame(
    pitch = unlist(tune$pitch, use.names = FALSE),
    duration = rep(tune$duration, times = rep_times),
    id = rep(tune$id, times = rep_times))
  
  notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)
  
  octave_matches <- gregexpr("[[:digit:]]+", tune$pitch)
  octave <- regmatches(tune$pitch, octave_matches)
  octave[lengths(octave)==0] <- 4
  tune$octave <- as.numeric(sapply(octave, `[[`, 1))
  
  pitch_remaining <- sapply(regmatches(tune$pitch, octave_matches, invert = TRUE), paste0, collapse = "")
  
  sharp_matches <- gregexpr("#", pitch_remaining)
  tune$sharp <- lengths(regmatches(pitch_remaining, sharp_matches))!=0
  
  pitch_remaining <- sapply(regmatches(pitch_remaining, sharp_matches, invert = TRUE), paste0, collapse = "")
  
  flat_matches <- gregexpr("b", pitch_remaining)
  tune$flat <- lengths(regmatches(pitch_remaining, flat_matches))!=0
  
  pitch_remaining <- sapply(regmatches(pitch_remaining, flat_matches, invert = TRUE), paste0, collapse = "")
  
  note_matches <- gregexpr("[ABCDEFG]", pitch_remaining)
  note <- regmatches(pitch_remaining, note_matches)
  note[lengths(note)==0] <- NA
  tune$note <- sapply(note, `[[`, 1)
  
  rm(pitch_remaining, octave_matches, sharp_matches, flat_matches, note_matches, octave, note)
  
  tune$note_numeric <- as.vector(notes[tune$note])
  tune$note_numeric <- tune$note_numeric + tune$sharp - tune$flat + tune$octave*12 + 12*(tune$note_numeric<3)
  
  tune$freq <- 2^((tune$note_numeric-60)/12)*440
  tune$freq[is.na(tune$freq)] <- 0
  
  make_sine <- function(freq, duration) {
    wave <- sin(seq(1/sample_rate, (duration/tempo)*60, 1/sample_rate)*freq*2*pi)
    fade <- seq(0, 1-(1/(fade_duration*sample_rate)), 1/(fade_duration*sample_rate))
    wave*c(fade, rep(1, length(wave)-2*length(fade)), rev(fade))
  }
  
  interior <- function(x, y) {
    val <- rowSums(mapply(make_sine, x, y, SIMPLIFY = TRUE, USE.NAMES = FALSE))
    max_val <- max(abs(val))
    if (max_val==0) {
      return(val)
    }
    return(val/max_val)
  }
  
  tune_wave <- unlist(mapply(FUN = interior, split(tune$freq, f = tune$id), split(tune$duration, f = tune$id), SIMPLIFY = FALSE))
  
  tune <- audio::audioSample(tune_wave, rate = sample_rate)
  
  if (is.null(output_path)) {
    audio::play(tune)
  } else {
    audio::save.wave(tune, output_path)
  }
}