class Badge {
    public String print(Integer id, String name, String department) {
        return new StringBuilder()
                .append(id == null ? "" : String.format("[%d] - ", id))
                .append(name + " - ")
                .append(department == null ? "OWNER" : department.toUpperCase())
                .toString();
    }
}
