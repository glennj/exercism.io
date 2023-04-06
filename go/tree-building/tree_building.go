package tree

import (
	"fmt"
	"sort"
)

type Record struct {
	ID     int
	Parent int
	node   *Node
}

type Node struct {
	ID       int
	Children []*Node
}

func (n *Node) AddChild(c *Node) {
	n.Children = append(n.Children, c)
}

func Build(records []Record) (*Node, error) {
	if len(records) == 0 {
		return nil, nil
	}

	sort.Slice(records, func(i, j int) bool {
		return records[i].ID < records[j].ID
	})

	if err := validateRecords(records); err != nil {
		return nil, fmt.Errorf("invalid input: %w", err)
	}

	for i, rec := range records {
		node := &Node{ID: rec.ID}
		records[i].node = node
		if i > 0 {
			records[rec.Parent].node.AddChild(node)
		}
	}

	return records[0].node, nil
}

func validateRecords(records []Record) error {
	for i, record := range records {
		if record.ID != i {
			return fmt.Errorf("id out of sequence: %d", record.ID)
		}
		if i == 0 {
			if records[0].Parent != 0 {
				return fmt.Errorf("root record must not have parent")
			}
		} else if record.ID <= record.Parent {
			return fmt.Errorf("a record with invalid parent: ID=%d, Parent=%d", record.ID, record.Parent)
		}
	}

	return nil
}

// bench
//
// BenchmarkTwoTree-2       	     206	   5930474 ns/op	 2883669 B/op	  131074 allocs/op
// BenchmarkTenTree-2       	    1399	    852027 ns/op	  568096 B/op	   15003 allocs/op
// BenchmarkShallowTree-2   	    1731	    705591 ns/op	  677720 B/op	   10022 allocs/op
