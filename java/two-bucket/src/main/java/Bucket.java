class Bucket {
    private final int size;
    private int used;
    private final String name;

    Bucket(String name, int size) {
        this.name = name;
        this.size = size;
    }

    int getSize()     { return size; }
    int getUsed()     { return used; }
    String getName()  { return name; }
    int getCapacity() { return size - used; }

    boolean isFull()  { return used == size; }
    boolean isEmpty() { return used == 0; }

    void fill()  { used = size; }
    void empty() { used = 0; }

    void add(int amount) { used += amount; }

    void pour(Bucket other) {
        int amount = Math.min(used, other.getCapacity());
        used -= amount;
        other.add(amount);
    }
}
