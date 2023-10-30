#include "robot_simulator.h"
#include <stddef.h>
#include <assert.h>
#include <math.h>

#ifndef M_PI
#define M_PI 3.1415926535
#endif

robot_status_t robot_create(robot_direction_t direction, int x, int y) {
    return (robot_status_t){direction, {x, y}};
}

static void turn(robot_status_t *robot, int dir) {
    robot->direction = (robot->direction + (dir * DIRECTION_INCREMENT) + DIRECTION_MAX) % DIRECTION_MAX;
}

static void advance(robot_status_t *robot) {
    float direction = (float)robot->direction * M_PI / (DIRECTION_MAX/2);
    robot->position.x += round(cos(direction));
    robot->position.y += round(sin(direction));
}

void robot_move(robot_status_t *robot, const char *commands) {
    assert(robot != NULL);
    assert(commands != NULL);

    for (; *commands; commands++) {
        switch (*commands) {
            case CMD_TURN_RIGHT: turn(robot, -1); break;
            case CMD_TURN_LEFT:  turn(robot, +1); break;
            case CMD_ADVANCE:    advance(robot);  break;
            default: break;
        }
    }
}
