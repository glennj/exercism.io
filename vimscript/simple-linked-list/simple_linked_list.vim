function! s:Node(value)
    let n = #{val: a:value, nxt: v:null}

    function n.Value()
        return self.val
    endfunction

    function n.Next(...)
        if a:0 == 0
            return self.nxt
        else
            let self.nxt = a:1
        endif
    endfunction

    return n
endfunction

" Creates a singly linked list that can only be traversed from head to tail.
" Each element points to the next element in the list.
"
function! SimpleLinkedList(values) abort
    let lst = #{head: v:null, length: 0}

    function lst.Head()
        if self.head is v:null
            throw 'The list is empty'
        endif
        return self.head
    endfunction

    function lst.Length()
        return self.length
    endfunction

    function lst.Push(value)
        let node = s:Node(a:value)
        eval node.Next(self.head)
        let self.head = node
        let self.length += 1
        return self
    endfunction

    function lst.Pop()
        if self.head is v:null
            throw 'The list is empty'
        endif
        let node = self.head
        let self.head = node.Next()
        eval node.Next(v:null)
        let self.length -= 1
        return node.Value()
    endfunction

    function lst.AsList()
        let result = []
        let node = self.head
        while node isnot v:null
            eval result->add(node.Value())
            let node = node.Next()
        endwhile
        return result
    endfunction

    function lst.Reversed()
        let rev = SimpleLinkedList([])
        let node = self.head
        while node isnot v:null
            eval rev.Push(node.Value())
            let node = node.Next()
        endwhile
        return rev
    endfunction

    for value in a:values
        call lst.Push(value)
    endfor

    return lst
endfunction
