# These variables are initialized on the command line (using '-v'):
# - size

BEGIN {
    x = y = 1
    dx = 0; dy = 1
    for (i = 1; i <= size * size; i++) {
        m[x][y] = i
        if ( x + dx < 1 || x + dx > size ||
             y + dy < 1 || y + dy > size ||
             m[x + dx][y + dy] != 0 ) 
        {
             tmp = dx
             dx = dy
             dy = -tmp        
        }
        x += dx
        y += dy
    }
    
    for (x = 1; x <= size; x++)
        for (y = 1; y <= size; y++)
            printf "%d%s", m[x][y], (y == size ? "\n" : " ")
}
