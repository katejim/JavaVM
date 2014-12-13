/**
 * Created by kate on 13.12.14.
 */

object MacroTest  extends  App{
  import PrintfMacro._
  printf("Character: %c\nString: %s\nFloat: %f\nInt: %d", 'C', "SimpleString", 12.4f, 12)
  //should fail
  //printf("String %f", "FailStirng")
}
