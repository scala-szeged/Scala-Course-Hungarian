
val list = List(1,2,3)
list.head // 0. elem
list.tail // a maradék az 1. elemtől kezdve

List(10).head // 10 -es szám
List(10).tail // üres lista

// :: az ami a lista elejére fűz
"a" :: List("b")


// metódus, más néven függvény definíciója
def darabold() { ??? }


// metódus és annak paramétere
// vasSzál - paraméter neve
// Int - paraméter típusa
def darabold(vasSzál: Int) { ??? }


// metódus
// írdFel - paraméterként átadott metódus
//          listát kap és semmit nem ad vissza
//          List[Int]     Unit
def darabold(írdFel: List[Int] => Unit) { ??? }


// A lista minden elemére külön println
list.foreach(println)

// összeg
list.sum

// méret, elemek száma
list.size