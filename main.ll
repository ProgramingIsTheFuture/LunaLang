; ModuleID = 'Dyri'
source_filename = "Dyri"

define i32 @hello(i32 %x) {
  ret i32 %x
}

define i32 @main() {
  %1 = call i32 @hello(i32 55)
  ret i32 %1
}

