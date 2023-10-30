#ifndef ROBOT_SIMULATOR_H
#define ROBOT_SIMULATOR_H

typedef enum {
   DIRECTION_EAST  = 0,
   DIRECTION_NORTH = 90,
   DIRECTION_WEST  = 180,
   DIRECTION_SOUTH = 270,
   DIRECTION_MAX   = 360,
   DIRECTION_INCREMENT = 90
} robot_direction_t;

typedef enum {
    CMD_TURN_RIGHT = 'R',
    CMD_TURN_LEFT  = 'L',
    CMD_ADVANCE    = 'A'
} commands_t;

typedef struct {
   int x;
   int y;
} robot_position_t;

typedef struct {
   robot_direction_t direction;
   robot_position_t position;
} robot_status_t;

robot_status_t robot_create(robot_direction_t direction, int x, int y);
void robot_move(robot_status_t *robot, const char *commands);

#endif
