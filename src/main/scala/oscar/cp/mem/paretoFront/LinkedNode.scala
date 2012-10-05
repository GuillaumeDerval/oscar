package oscar.cp.mem.paretoFront

class LinkedNode(v : Int, l : OrderedLinkedList) {
	
	private var p : LinkedNode = null
	private var n : LinkedNode = null
	
	def list  = l
	def value = v
	
	def prev = p
	def next = n
	
	def prev_= (x : LinkedNode) { p = x }
	def next_= (x : LinkedNode) { n = x }

	def isFirst = p == null
	def isLast  = n == null
}