

object Application extends App {
  // ********************************************************************************************************************************************************************************************
  //                                                               Основы работы с функциями в Scala
  // ********************************************************************************************************************************************************************************************
  //-------------------------------------------------------------
  // Фундаментальная концепция функционального Scala заключается в том, что функции являются по умолчанию функциями "первого класса" (first-class functions или first-class citizen)
  // и могут обрабатываться как константы, то есть:
  //      - присваиваться переменным (или инициализировать другие константы);
  //      - передаваться в качестве аргумента другим функциям;
  //      - возвращается в качестве значения из других функций.
  // Scala по умолчанию рассматривает все функции как функции первого класса.

  // Концепция функции высшего порядка (ФВП) тесно связана с концепцией функции первого класса.
  // Функция высшего порядка имеет по крайней мере одно из следующих свойств:
  //     - принимает одну или несколько функций в качестве параметров,
  //     - возвращает функцию как результат.
  // С ФВП функции обрабатываются так же, как и любые другие типы значений.

  //--------------------------------------------------------------------------------------------
  // Объявление функций в Scala
  def m_sum(x: Int, y: Int): Int = x + y // Функция сложения двух чисел

  def calcAnything(number: Int, calcFunction: Int => Int): Int = calcFunction(number) // Функция, возвращающая результат "какого-то вычислиения" Int => Int (функция, содержащая реализацию вычислиения, передается в качестве аргумента)

  def calcSquare(num: Int): Int = num * num // Функция, возвращающая квадрат целого значения

  def calcCube(num: Int): Int = num * num * num // Функция, возвращающая куб целого значения

  def performAddition(x: Int, y: Int): Int = x + y // Еще одна Функция сложения двух чисел

  def performSubtraction(x: Int, y: Int): Int = x - y // Функция вычитания двух чисел

  def performMultiplication(x: Int, y: Int): Int = x * y // Функция умножения двух чисел

  // Объявление функций в Scala и передача ее как константы
  val f_sum: (Int, Int) => Int = (x, y) => x + y // Функция сложения двух чисел как значение

  //--------------------------------------------------------------------------------------------
  // передача функции calcSquare как значения
  val squareCalculated = calcAnything(2, calcSquare)
  assert(squareCalculated == 4) // программа выкидывает Assertion error, если функция вернула результат, отличный от указанного (используется для прогонки тестов)

  val cubeCalculated = calcAnything(3, calcCube)
  assert(cubeCalculated == 27) // программа выкидывает Assertion error, если функция вернула результат, отличный от указанного (используется для прогонки тестов)

  //--------------------------------------------------------------------------------------------
  // функция, возвращающая другую функцию как значение
  def performArithmeticOperation(num1: Int, num2: Int, operation: String): Int = {
    operation match {
      case "addition" => performAddition(num1, num2)
      case "subtraction" => performSubtraction(num1, num2)
      case "multiplication" => performMultiplication(num1, num2)
      case _ => -1
    }
  }

  val additionResult = performArithmeticOperation(2, 4, "addition")
  assert(additionResult == 6)

  val subtractionResult = performArithmeticOperation(10, 6, "subtraction")
  assert(subtractionResult == 4)

  val multiplicationResult = performArithmeticOperation(8, 5, "multiplication")
  assert(multiplicationResult == 40)

  // ********************************************************************************************************************************************************************************************
  // Каррированная функция сложения двух чисел
  val f_curriedSum: Int => Int => Int = x => y => x + y // явное объявление
  val f_curriedSum2: Int => Int => Int = f_sum.curried // объявление с использованием метода каррирования функции нескольких аргументов

  //--------------------------------------------------------------
  // Пример каррированного метода
  def m_curriedSum(x: Int)(y: Int): Int = x + y // явное объявление

  val m_curriedSum2: Int => Int => Int = (m_sum _).curried // объявление с использованием метода каррирования другого метода с несколькими аргументами
  //--------------------------------------------------------------
  // Partial application
  // объявление функции/метода, являющейся частным случаем более общей (прибавление 1 или инкрементирование)
  val pa_f_increment: Int => Int = f_curriedSum(1) // объявление частного случая общей функции
  val pa_m_increment: Int => Int = m_curriedSum(1) // объявление частного случая общего метода
  //--------------------------------------------------------------
  // Секция вывода результатов выполнения объявленных функций
  println(f_sum(1, 2))
  println(f_curriedSum(1)(2))
  println(f_curriedSum2(1)(2))
  println(m_curriedSum(1)(2))
  println(m_curriedSum2(1)(2))
  println(pa_f_increment(8))
  println(pa_m_increment(8))

  //**********************************************************************************
  //   Цикл for
  //**********************************************************************************

  // Общая запись цикла for:     for ( receiver <- generator ) { statement }
  //    receiver - переменная, которая получает следующее новое значение от генератора на каждой итерации
  //    generator - в базовых случаях (Range, Collection, Map)
  //-------------------------------------------------------------------------------------------------------
  //----------------------------------------------------------
  //  generator - Range (диапазон значений, который используется для определения количества шагов цикла)
  //                    (задает режим цикла со счетчиком)
  val range = 1 to 3 // счетчик пробегает значения от 1 до 3 включительно
  val rangeUntil = 1 until 3 // счетчик пробегает значения от 1 до 3 не включительно

  // Секция выполнения цикла
  for (num <- range) {
    println(num)
  }
  //...............................
  for (num <- rangeUntil) {
    println(num)
  }
  //...............................
  // В цикле можно использовать несколько генераторов, это позволит определить вложенные циклы
  // Пример вложенного цикла
  for {
    i <- range
    j <- rangeUntil
  } {
    println(s"$i, $j")
  }
  //----------------------------------------------------------
  //  generator - Collection (различные реализации коллекций)
  val degreeList = Seq("кандидат наук", "доктор наук", "без степени")
  for (degree <- degreeList) {
    println(degree)
  }

  // Вывод различных последовательнстей проведения занятий
  // Каждая строка - это конкатенация разных дисциплин, разделенных запятой
  val subjectList = Seq("Информатика", "Программирование", "Схемотехника")
  for {
    subj1 <- subjectList
    subj2 <- subjectList
    if subj2 != subj1 // фильтруем, чтобы не было нескольких одинаковых дисциплин
    subj3 <- subjectList
    if subj3 != subj2 && subj3 != subj1 // фильтруем, чтобы не было нескольких одинаковых дисциплин
  } {
    println(s"$subj1, $subj2, $subj3 ") // не чистая итерация (побочный эффект - печать)
  }
  //----------------------------------------------------------
  //  generator - Map (различные реализации <Ключ, Значение>)

  val degreeMap = Map("кн" -> "кандидат наук", "дн" -> "доктор наук", "бс" -> "без степени")
  for ((key, value) <- degreeMap) {
    println(s"$key - это аббревиатура степени $value")
  }

  //Если дана карта списков, то используются несколько генераторов.
  // Во внешней итерации получаем пару "ключ-значение", причем для внутренней итерации
  // значение является списком.
  val deck = Map("♣" -> List("Туз", "Король", "Дама"),
    "♦" -> List("Валет", "10"),
    "♥" -> List("9", "8", "7"),
    "♠" -> List("Туз", "Король", "Валет", "6"))
  for {
    (suit, cardList) <- deck
    card <- cardList
  } {
    println(s"$card имеет масть $suit") // не чистая итерация (побочный эффект - печать)
  }

  // Определение правила изменения значинией элементов коллекции
  //               val result = for ( generator ) yield {  yield_statement  }
  // yield - вернет результат выполнения оператора в качестве нового элемента результирующей коллекции

  // Меняем содержимое коллекции
  val numberList = List(1, 2, 3) // Список целых чисел
  val equationList = for (number <- numberList) yield {
    s"""$number + $number = ${number + number}""" // чистая итерация
  }
  // Выводим содержимое измененной коллекции
  for (el <- equationList) {
    println(el)
  }

  // В Scala все Collection и Map предоставляют реализации для функций map и flatMap.
  // В теории категорий такие контейнеры называются - Монады.
  // В языке Scala монадами являются: Option, Either и Future.
  // Все они реализуют как функцию map, так и функцию flatMap.

  // Объявлены две Option константы
  val someIntValue = Some(10)
  val someStringValue = Some("Ten")

  // Вариант 1 (без синтаксического сахара, напрямую вызываем flatMap и map)
  val _result = someIntValue.flatMap(
    intValue => someStringValue.map(
      stringValue => s"$intValue это $stringValue")
  )

  println(_result)

  // Вариант 2 (с синтаксическим сахаром)
  // Можно использовать режим работы for (for-comprehension) с несколькими генераторами,
  // поскольку Option в Scala - это монада и поддерживает map и flatMap

  val result = for { // result: Option[String] = Some(10 is Ten)
    intValue <- someIntValue
    stringValue <- someStringValue
                     } yield {
    s"$intValue это $stringValue" // чистая итерация
  }

  println(result)

  def twoFuncCompose[A, B, C](f1: A => B)(f2: B => C): A => C = a => f2(f1(a))

  def threeFuncCompose0[A, B, C, D](f1: A => B)(f2: B => C)(f3: C => D): A => D = a => f3(f2(f1(a)))


  val v1 = List(1, 2, 3)

  val v2 = List(1, 2, 3)


  def join: List[Int] => List[Int] => List[(Int, Int)] = l1 => l2 => l1.zip(l2) // список => список => список пар

  println(join(v1)(v2))


  def multiply(l: List[(Int, Int)]): List[Int] = {


    def run(l: List[(Int, Int)], acc: List[Int]): List[Int] = {
      l match {
        case Nil => acc
        case h :: t => run(t, h._1 * h._2 :: acc)

      }

    }

    run(l, Nil)

  }


  def sum(l: List[Int]): Int = l.sum


  def scalarProduct: List[Int] => List[Int] => Int = a => twoFuncCompose(twoFuncCompose(join(a))(multiply))(sum)

  def scalarProduct1: List[Int] => List[Int] => Int = a => threeFuncCompose0(join(a))(multiply)(sum)

  println(scalarProduct1(v1)(v2))

  def NOD: Int => Int => Int = a => b => {
    val x = a max b
    val y = a min b
    x % y match {
      case 0 => y
      case _ => NOD(x % y)(y)
    }
  }

  def NOK: Int => Int => Int = a => b => a / NOD(a)(b) * b

  def printNODNOK(a: Int, b: Int) = {
    println(s"НОД($a,$b) = ${NOD(a)(b)}")
    println(s"НОК($a,$b) = ${NOK(a)(b)}")
  }

  printNODNOK(30, 18)

  def LetMeIIIIN: Iterable[Iterable[Int] => Iterable[Int]] => Int =>
      Iterable[Iterable[Int]] = A => n =>
    for (func <- A) yield func(1 to n)

  def LetMeIN(A:Iterable[Iterable[Int] => Iterable[Int]])(n:Int): Iterable[Iterable[Int]] =
    for (func <- A) yield func(1 to n)

  def filterThanMap[A, B](Filter: A => Boolean)(Map: A => B): (Iterable[A] => Iterable[B]) = a=>a.view.filter(Filter).map(Map)

  def neutural1 = filterThanMap[Int, Int](a => true)(a => a)

  def odd1 = filterThanMap[Int, Int](_ % 2 == 1)(a => a)

  def even1 = filterThanMap[Int, Int](_ % 2 == 0)(a => a)

  def factor1 = filterThanMap[Int, Int](a => true)(i => (1 to i).product)

  def sqr1 = filterThanMap[Int, Int](a => true)(i => i * i)

  def justMap = filterThanMap[Int, Int](a => true)(_)

  def pwr21 = justMap(a => math.pow(2, a).toInt)

  def neutural: Iterable[Int] => Iterable[Int] = a => a

  def odd: Iterable[Int] => Iterable[Int] = _.filter(_ % 2 == 1)

  def even: Iterable[Int] => Iterable[Int] = for (i <- _ if i % 2 == 0) yield i //Ну, честно, это неэлегантно

  def factor: Iterable[Int] => Iterable[Int] = _.map(i => (1 to i).product)

  def sqr: Iterable[Int] => Iterable[Int] = _.map(i => i * i)

  def pwr2: Iterable[Int] => Iterable[Int] = _.map(i => math.pow(2, i).toInt)

  println(s"answers:\n${LetMeIIIIN(List(neutural, odd, even, factor, sqr, pwr2))(10)}")

  /**
   * Это комент, чтобы объяснить строчку ниже
   * groupBy - группирует общекты коллекции по свойству, в данном случае свойством будет само значение, но вообще можно
   * впихнуть любую функцию. В резульатате мы получаем отображение
   * Int=>Iterable[Int]
   * .map - создаём новую коллекцию из элементов коллекции парочек получившейся на предыдущем шаге.
   * (Пары (Int,Iterable[Int]))
   * _._1 - из парочки вытаскиваем 1 элемент, то есть наше уникальное число.
   * В общем-то это тоже самое, что и оставленно в итоге, просто случай более общий
   */
  //def uniqueEven: Iterable[Int]=>Iterable[Int] = _.groupBy(a=>a).map(_._1).filter(_%2==1)

  def uniqueEven: List[Int] => List[Int] = _.filter(_ % 2 == 0).distinct

  def sortTwice: List[Int] => (List[Int], List[Int]) = a => (a.sortWith(_ > _), a.sortWith(_ < _))

  def fourPointOne: List[Int] => (List[Int], List[Int]) = twoFuncCompose(uniqueEven(_))(sortTwice)

  val list = List(1, 1, 1, 2, 3, 3, 3, 5, 6, 7, 8, 9, 0, 0) //
  println(s"results 4.1:\nchanged from $list to\n${fourPointOne(list)}")

  def substitute: Char => Char => String => String = C => H => str => for (ch <- str) yield ch match {
    case C => H
    case ch:Char => ch
  }

  val str = "Wello hold"
  val ch0 = 'l'
  val ch1 = 'b'
  println(s"results 4.2:\n In string \"$str\" $ch0 replaced with $ch1:\n ${substitute(ch0)(ch1)(str)}")

  //принимает функцию преобразования символа и строку. Преобразует каждый символ строки
  def charTransformer: (Char => Char) => String => String = fch => str => for (ch <- str) yield fch(ch)

  val charTransformerGen: Char => Char => (Char => Char) = ch0 => ch1 => {
    ch => if(ch == ch0) ch1 else ch
  }
  println("abcdd".map(charTransformerGen('d')('e')))
  println(charTransformer(charTransformerGen('d')('e'))("Dat date is dat bad"))

  /**
   * Собственно композиция.
   */
  import Composition.SimpleFunctionComposition._
  val f1:Int=>Int = _+1
  val f2:Int=>String = _.toString
  val f3:String=>Array[Char] = _.toCharArray
  val f4:Array[Char]=>String = _.mkString("")
  val f5:Array[Char]=>Array[Char] = _.map(c=>(c+1).toChar)

  val f6 =
    f1:=>f2:=>f3:=>f5:=>f4:=>println
  f6(9875)

  def threeV2(funcs:Seq[PartialFunction[Int,Int]])(n:Int): Seq[Seq[Int]] =
    funcs.map(func =>(1 to n).collect(func))

  type PFInt2Int = PartialFunction[Int,Int]

  def neutral2:PFInt2Int = {
    case i:Int => i
  }
  def odd2:PFInt2Int = {
    case i:Int if i % 2 == 1 => i
  }
  def even2:PFInt2Int = {
    case i:Int if i % 2 == 0 => i
  }
  def factor2:PFInt2Int = {
    case i:Int => (1 to i).product
  }
  def sqr2:PFInt2Int = {
    case i:Int => i*i
  }
  def twoPwr2:PFInt2Int = {
    case i:Int => math.pow(2,i).toInt
  }
  val funcSeq = Seq(neutral2,odd2,even2,factor2,sqr2,twoPwr2)
  val threeV3 = threeV2(_)(11)
  println(threeV3(funcSeq).map(c=> c.mkString(",")).mkString("\n"))
}

// 3. Определите функцию, принимающую на вход целое n и возвращающую список, содержащий n элементов, упорядоченных по возрастанию.
//    - список натуральных чисел,
//    - список нечётных натуральных чисел,
//    - список чётных натуральных чисел,
//    - список квадратов натуральных чисел,
//    - список факториалов,
//    - список степеней 2^i.

// 4. Определите следующие функции:
//   4.1 Функция removeOdd, которая удаляет из заданного списка целых чисел все нечётные числа.
//       Например removeOdd [1,4,5,6,10,6] должен возвращать [4,10], убрать дубликаты + вернуть 2 списка,
//       отсортированных в прямом и обратном  порядке.
//   4.2 Функция substitute :: Char -> Char -> String -> String, которая заменяет в строке указанный символ на заданный.
//       Пример: substitute ‘e’ ‘i’ “eigenvalue” возвращает “iiginvalui”.