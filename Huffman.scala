package encoding

import common._

object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class HuffmanTree
  case class Fork(left: HuffmanTree, right: HuffmanTree, chars: List[Char], weight: Int) extends HuffmanTree
  case class Leaf(char: Char, weight: Int) extends HuffmanTree
  
  def weight(tree: HuffmanTree): Int = tree match {
      case Leaf(ch,w)=> w
      case Fork(left, right, chList, w) => w
  }
  
  def chars(tree: HuffmanTree): List[Char] = tree match {
      case Leaf(ch,w) => List(ch)
      case Fork(left, right, chList, w) => chList
  }
  
  def assembleCodeTree(left: HuffmanTree, right: HuffmanTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  val sampleTree = assembleCodeTree(
    assembleCodeTree(Leaf('x', 1), Leaf('e', 1)),
    Leaf('t', 2)
  )

  // Building the Huffman trees
  def frequencyOfChars(chars: List[Char]): List[(Char, Int)] = chars.groupBy(x => x.toLower).map(x=>(x._1, x._2.length)).toList
  
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.sortWith((x1,x2)=> x1._2 < x2._2).map((x) => Leaf(x._1,x._2))
   
  def isSingleton(trees: List[HuffmanTree]): Boolean = trees.size == 1
  
  /**
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list is returned unchanged.
   */
    def combine(trees: List[HuffmanTree]): List[HuffmanTree] = trees match {
    case left::right::rest => (assembleCodeTree(left, right)::rest).sortWith((x1,x2)=>weight(x1) < weight(x2))
    case _ => trees
  }
  
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   */
   def until(x: List[HuffmanTree]=>Boolean, y: List[HuffmanTree]=>List[HuffmanTree])(trees:List[HuffmanTree]): List[HuffmanTree] = if(x(trees)) trees else until(x,y)(y(trees))

   def createHuffmanTree(chars: List[Char]): HuffmanTree = until(isSingleton, combine)( makeOrderedLeafList(frequencyOfChars(chars)) ).head
   // Decoding

   type Bit = Int

   def decode(tree: HuffmanTree, bits: List[Bit]): List[Char] = {
   def traverse(remaining: HuffmanTree, bits: List[Bit]): List[Char] = remaining match {
     case Leaf(c, _) if bits.isEmpty => List(c)
     case Leaf(c, _) => c :: traverse(tree, bits)
     case Fork(left, right, _, _) if bits.head == 0 => traverse(left, bits.tail)
     case Fork(left, right, _, _) => traverse(right, bits.tail)
    }

    traverse(tree, bits)
   }
  
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
   val frenchCode: HuffmanTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

  /**
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
   val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)

   def decodedSecret: List[Char] = decode(frenchCode, secret)

   // Encoding using code table

   type CodeTable = List[(Char, List[Bit])]

  
   
   /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character. Using the code tables of the
   * sub-trees, build the code table for the parent and recursively the entire tree.
   */
   def convert(tree: HuffmanTree): CodeTable = tree match {
     case Leaf(c, w) => List( (c, List()) )
     case Fork(left, right, cs, w) => mergeCodeTables(convert(left), convert(right))
   }
  
   type Code = (Char, List[Bit])

   def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = {
     def prepend(b: Bit)(code: Code): Code = (code._1, b :: code._2)
     a.map(prepend(0)) ::: b.map(prepend(1))
   }
    
    /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
   def codeBits(table: CodeTable)(char: Char): List[Bit] = {
      table.filter( (code) => code._1 == char ).head._2
   }
  
   def quickEncode(tree: HuffmanTree)(text: List[Char]): List[Bit] = text flatMap codeBits(convert(tree))
}
