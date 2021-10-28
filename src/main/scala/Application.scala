import scala.collection.immutable.HashSet
import scala.math.Ordered.orderingToOrdered

object Application extends App{
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
  def m_sum(x: Int, y: Int): Int = x + y                                                            // Функция сложения двух чисел

  def calcAnything(number: Int, calcFunction: Int => Int): Int = calcFunction(number)               // Функция, возвращающая результат "какого-то вычислиения" Int => Int (функция, содержащая реализацию вычислиения, передается в качестве аргумента)
  def calcSquare(num: Int): Int = num * num                                                         // Функция, возвращающая квадрат целого значения
  def calcCube(num: Int): Int = num * num * num                                                     // Функция, возвращающая куб целого значения

  def performAddition(x: Int, y: Int): Int = x + y                                                  // Еще одна Функция сложения двух чисел
  def performSubtraction(x: Int, y: Int): Int = x - y                                               // Функция вычитания двух чисел
  def performMultiplication(x: Int, y: Int): Int = x * y                                            // Функция умножения двух чисел

  // Объявление функций в Scala и передача ее как константы
  val f_sum: (Int, Int) => Int = (x, y) => x + y                                                    // Функция сложения двух чисел как значение

  //--------------------------------------------------------------------------------------------
  // передача функции calcSquare как значения
  val squareCalculated = calcAnything(2, calcSquare)
  assert(squareCalculated == 4)         // программа выкидывает Assertion error, если функция вернула результат, отличный от указанного (используется для прогонки тестов)

  val cubeCalculated = calcAnything(3, calcCube)
  assert(cubeCalculated == 27)          // программа выкидывает Assertion error, если функция вернула результат, отличный от указанного (используется для прогонки тестов)

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
  println(f_sum(1,2))
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
  val range = 1 to 3           // счетчик пробегает значения от 1 до 3 включительно
  val rangeUntil = 1 until 3   // счетчик пробегает значения от 1 до 3 не включительно

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
    println (s"$i, $j")
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
    if subj2 != subj1                    // фильтруем, чтобы не было нескольких одинаковых дисциплин
    subj3 <- subjectList
    if subj3 != subj2 && subj3 != subj1  // фильтруем, чтобы не было нескольких одинаковых дисциплин
  } {
    println(s"$subj1, $subj2, $subj3 ")          // не чистая итерация (побочный эффект - печать)
  }
  //----------------------------------------------------------
  //  generator - Map (различные реализации <Ключ, Значение>)

  val degreeMap = Map("кн" -> "кандидат наук", "дн" -> "доктор наук", "бс" -> "без степени")
  for ((key,value) <- degreeMap) {
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
      println(s"$card имеет масть $suit")         // не чистая итерация (побочный эффект - печать)
    }

  // Определение правила изменения значинией элементов коллекции
  //               val result = for ( generator ) yield {  yield_statement  }
  // yield - вернет результат выполнения оператора в качестве нового элемента результирующей коллекции

  // Меняем содержимое коллекции
  val numberList = List(1, 2, 3)   // Список целых чисел
  val equationList = for (number <- numberList) yield {
    s"""$number + $number = ${number + number}"""             // чистая итерация
  }
  // Выводим содержимое измененной коллекции
  for (el <- equationList){
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

  val result = for {                    // result: Option[String] = Some(10 is Ten)
    intValue <- someIntValue
    stringValue <- someStringValue
  } yield {
    s"$intValue это $stringValue"       // чистая итерация
  }

  println(result)

  def twoFuncCompose[A,B,C](f1: A => B)(f2: B => C):A => C = a => f2(f1(a))

  def threeFuncCompose0[A,B,C,D](f1: A => B)(f2: B => C)(f3: C=>D):A => D = a => f3(f2(f1(a)))


  val v1 = List(1,2,3)

  val v2 = List(1,2,3)



  def join: List[Int] => List[Int] => List[(Int,Int)] = l1 => l2 => l1.zip(l2) // список => список => список пар

  println(join(v1)(v2))



  def multiply(l: List[(Int, Int)]): List[Int] = {


    def run(l: List[(Int, Int)], acc: List[Int]): List[Int] = {
      l match {
        case Nil => acc
        case h :: t => run(t,h._1*h._2 :: acc)

      }

    }

    run(l,Nil)

  }



  def sum (l:List[Int]): Int = l.sum



  def scalarProduct: List[Int] => List[Int] => Int = a => twoFuncCompose(twoFuncCompose(join(a))(multiply))(sum)
  def scalarProduct1: List[Int] => List[Int] => Int = a => threeFuncCompose0(join(a))(multiply)(sum)
  println(scalarProduct1(v1)(v2))

  def NOD: Int=>Int=>Int = a => b => {
    val x = a max b
    val y = a min b
    x%y match {
      case 0 => y
      case _ => NOD(x%y)(y)
    }
  }
  def NOK:Int=>Int=>Int = a=>b=> a/NOD(a)(b) * b
  def printNODNOK(a:Int,b:Int) = {
    println(s"НОД($a,$b) = ${NOD(a)(b)}")
    println(s"НОК($a,$b) = ${NOK(a)(b)}")
  }
  printNODNOK(30,18)

  def LetMeIIIIN: Iterable[Iterable[Int]=>Iterable[Int]]=>Int=>Iterable[Iterable[Int]] = A=>n=>
    for(func<-A) yield func(1 to n)
    //A=>n=> for(func<-A) yield (for(i<-1 to n) yield func(i)).sortWith(_ > _)

  def neutural: Iterable[Int]=>Iterable[Int] = _.map(i=>i)
  def odd: Iterable[Int]=>Iterable[Int] = for(i<-_ if i%2==1) yield i   //Лучше использовать фильтр в данном случае.
  def even: Iterable[Int]=>Iterable[Int] = for(i<-_ if i%2==0) yield i  //Ну, честно, это неэлегантно
  def factor: Iterable[Int]=>Iterable[Int] = _.map(i=> (1 to i).product)
  def sqr: Iterable[Int]=>Iterable[Int] = _.map(i=>i*i)
  def pwr2: Iterable[Int]=>Iterable[Int] = _.map(i=> math.pow(2,i).toInt)
  println(s"answers:\n${LetMeIIIIN(Array(neutural,odd,even,factor,sqr,pwr2))(10).mkString("\n")}")
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
  def uniqueEven: Iterable[Int]=>Iterable[Int] = _.groupBy(a=>a).map(_._1).filter(_%2==1)

  def uniqueEven: List[Int]=>List[Int] = new HashSet[Int].concat(_).toList.filter(_%2==1)
  def sortTwice: List[Int]=>(List[Int],List[Int]) = a=>(a.sortWith(_>_),a.sortWith(_<_))
  def fourPointOne:List[Int]=>(List[Int],List[Int]) = twoFuncCompose(uniqueEven(_))(sortTwice)
  val list = List(1,1,1,2,3,3,3,5,6,7,8,9,0,0)      //
  println(s"results 4.1:\nchanged from $list to\n${fourPointOne(list)}")
  def substitute:Char=>Char=>String=>String = C=>H=>str=> for(ch<-str) yield ch match{
    case C => H
    case c:Char => c
  }
  val str = "Wello hold"
  val ch0 = 'l'
  val ch1 = 'b'
  println(s"results 4.2:\n In string \"$str\" $ch0 replaced with $ch1:\n ${substitute(ch0)(ch1)(str)}")
}