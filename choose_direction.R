choose_direction <- function(pos, dot_pos) {
  if (pos[1,1] < dot_pos[1]) return('d')
  if (pos[1,1] > dot_pos[1]) return('a')
  if (pos[1,2] < dot_pos[2]) return('w')
  if (pos[1,2] > dot_pos[2]) return('s')
}