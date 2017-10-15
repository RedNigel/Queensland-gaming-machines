
transition_row <- function(df, tween_cols, nframes, frame_offset = 0){
  if(!(nrow(df) == 2)){
    stop("I make transitions between 2 rows")
  }

  df_const_cols <- df[, -match(tween_cols, names(df)) ]
  df_tween_cols <- df[, tween_cols]

  tween_rows <- tween_states(
    data = list(df_tween_cols[1,], df_tween_cols[2,]),
    tweenlength = 1,
    statelength = 1,
    ease = "cubic-in-out",
    nframes = nframes
  )

  transition_df <-
    rbind(
      cbind(tween_rows[ seq(nrow(tween_rows)-1) ,], df_const_cols[1,]),
      cbind(tween_rows[ nrow(tween_rows), ], df_const_cols[2,])
    )
  transition_df$.frame <- transition_df$.frame + frame_offset
  transition_df
}

start_row_shadow <- function(df, shadow_of_col = NULL, shadow_length = nrow(df)){
  shadow <- df[rep(1,shadow_length),]
  shadow$.frame <- seq(min(df$.frame),length.out = shadow_length, by = 1)
  shadow[, shadow_of_col] <-  paste0("000_",shadow[, shadow_of_col][[1]][[1]],"_shadow")
  shadow
}

