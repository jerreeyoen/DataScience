import com.oracle.xmlns.internal.webservices.jaxws_databinding.JavaWsdlMappingType.JavaMethods

object Hello extends App {
  println("Hello, World!")
  def sayHello(FirstName: String, middleName: String ="", lastName: String = "")= print(FirstName + " " + middleName + " " + lastName)

  sayHello("Jerry")
// Compare to Java, Scala is easier to define default value. No need to do many ifs check and default value assign.
//in Java
//  public void sayHello(String name, int age){
//    if (name == null){
//  name = "defaultVal"
//}
//  if (age  == 0){
//  age = defaultage}
//}

  // testing with parameters ordered changed
  println("")
  sayHello(lastName = "Yang", FirstName = "Jerry")
  }