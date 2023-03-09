// Package census simulates a system used to collect census data.
package census

// Resident represents a resident in this city.
type Resident struct {
	Name    string
	Age     int
	Address map[string]string
}

// NewResident registers a new resident in this city.
func NewResident(name string, age int, address map[string]string) *Resident {
	return &Resident{
		Name:    name,
		Age:     age,
		Address: address,
	}
}

// HasRequiredInfo determines if a given resident has all of the required information.
func (r *Resident) HasRequiredInfo() bool {
	// `r.Address["street"] == ""` covers 3 situations:
	// - the "street" key is present in the map but the value is empty
	// - the "street" key is not present
	// - the map itself is nil
	//
	// For the 2nd and 3rd situtions, key lookups return the map's value type's zero value

	return r.Name != "" && r.Address["street"] != ""
}

// Delete deletes a resident's information.
func (r *Resident) Delete() {
	// fails: can't change the actual pointer, only the value pointed at
	// r = &Resident{}

	// passes, but verbose
	// r.Name = ""
	// r.Age = 0
	// r.Address = nil

	// assigning a new value at the pointer
	*r = Resident{}
}

// Count counts all residents that have provided the required information.
func Count(residents []*Resident) int {
	var count int
	for _, resident := range residents {
		if resident.HasRequiredInfo() {
			count += 1
		}
	}
	return count
}
