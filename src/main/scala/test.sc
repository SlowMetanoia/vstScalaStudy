

def isA:String=>String = {
  case "A" => "A"
  case _=>"not A"
}
isA("A")
isA("V")