; ModuleID = 'HeBGB'
source_filename = "HeBGB"

declare i32 @print_int(i32)

declare i32 @print_string(i8*)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ([1 x i8*]* getelementptr ([1 x i8*], [1 x i8*]* null, i32 1) to i32))
  %clo = bitcast i8* %malloccall to [1 x i8*]*
  %f_ptr_loc = getelementptr inbounds [1 x i8*], [1 x i8*]* %clo, i32 0, i32 0
  store i8* bitcast (i8* (i8*, i8*)* @add_three to i8*), i8** %f_ptr_loc, align 8
  %clo_ptr = bitcast [1 x i8*]* %clo to i8*
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp = bitcast i8* %malloccall1 to i32*
  store i32 3, i32* %tmp, align 4
  %cell = bitcast i32* %tmp to i8*
  %malloccall2 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp3 = bitcast i8* %malloccall2 to i32*
  store i32 2, i32* %tmp3, align 4
  %cell4 = bitcast i32* %tmp3 to i8*
  %malloccall5 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp6 = bitcast i8* %malloccall5 to i32*
  store i32 1, i32* %tmp6, align 4
  %cell7 = bitcast i32* %tmp6 to i8*
  %clo_ptr8 = bitcast i8* %clo_ptr to i8* (i8*, i8*)**
  %clo_val = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %clo_ptr8, align 8
  %f_result = call i8* %clo_val(i8* %cell7, i8* %clo_ptr)
  %clo_ptr9 = bitcast i8* %f_result to i8* (i8*, i8*)**
  %clo_val10 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %clo_ptr9, align 8
  %f_result11 = call i8* %clo_val10(i8* %cell4, i8* %f_result)
  %clo_ptr12 = bitcast i8* %f_result11 to i8* (i8*, i8*)**
  %clo_val13 = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %clo_ptr12, align 8
  %f_result14 = call i8* %clo_val13(i8* %cell, i8* %f_result11)
  %tmp15 = bitcast i8* %f_result14 to i32*
  %tmp16 = load i32, i32* %tmp15, align 4
  %print_int = call i32 @print_int(i32 %tmp16)
  %malloccall17 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp18 = bitcast i8* %malloccall17 to i32*
  store i32 0, i32* %tmp18, align 4
  %cell19 = bitcast i32* %tmp18 to i8*
  %tmp20 = bitcast i8* %cell19 to i32*
  %ret_code = load i32, i32* %tmp20, align 4
  ret i32 %ret_code
}

define i8* @add_three(i8* %a, i8* %curr_clo) {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ([2 x i8*]* getelementptr ([2 x i8*], [2 x i8*]* null, i32 1) to i32))
  %clo = bitcast i8* %malloccall to [2 x i8*]*
  %f_ptr_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo, i32 0, i32 0
  store i8* bitcast (i8* (i8*, i8*)* @add_two to i8*), i8** %f_ptr_loc, align 8
  %clo_ptr = bitcast [2 x i8*]* %clo to i8*
  %var_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo, i32 0, i32 1
  store i8* %a, i8** %var_loc, align 8
  ret i8* %clo_ptr
}

define i8* @add_two(i8* %b, i8* %curr_clo) {
entry:
  %clo_arr_ptr = bitcast i8* %curr_clo to [2 x i8*]*
  %id_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo_arr_ptr, i32 0, i32 1
  %id_ptr = load i8*, i8** %id_loc, align 8
  %malloccall = tail call i8* @malloc(i32 ptrtoint ([3 x i8*]* getelementptr ([3 x i8*], [3 x i8*]* null, i32 1) to i32))
  %clo = bitcast i8* %malloccall to [3 x i8*]*
  %f_ptr_loc = getelementptr inbounds [3 x i8*], [3 x i8*]* %clo, i32 0, i32 0
  store i8* bitcast (i8* (i8*, i8*)* @add_one to i8*), i8** %f_ptr_loc, align 8
  %clo_ptr = bitcast [3 x i8*]* %clo to i8*
  %var_loc = getelementptr inbounds [3 x i8*], [3 x i8*]* %clo, i32 0, i32 1
  store i8* %b, i8** %var_loc, align 8
  %var_loc1 = getelementptr inbounds [3 x i8*], [3 x i8*]* %clo, i32 0, i32 2
  store i8* %id_ptr, i8** %var_loc1, align 8
  ret i8* %clo_ptr
}

define i8* @add_one(i8* %c, i8* %curr_clo) {
entry:
  %clo_arr_ptr = bitcast i8* %curr_clo to [2 x i8*]*
  %id_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo_arr_ptr, i32 0, i32 1
  %id_ptr = load i8*, i8** %id_loc, align 8
  %clo_arr_ptr1 = bitcast i8* %curr_clo to [3 x i8*]*
  %id_loc2 = getelementptr inbounds [3 x i8*], [3 x i8*]* %clo_arr_ptr1, i32 0, i32 2
  %id_ptr3 = load i8*, i8** %id_loc2, align 8
  %clo_arr_ptr4 = bitcast i8* %curr_clo to [3 x i8*]*
  %id_loc5 = getelementptr inbounds [3 x i8*], [3 x i8*]* %clo_arr_ptr4, i32 0, i32 2
  %id_ptr6 = load i8*, i8** %id_loc5, align 8
  %clo_arr_ptr7 = bitcast i8* %curr_clo to [2 x i8*]*
  %id_loc8 = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo_arr_ptr7, i32 0, i32 1
  %id_ptr9 = load i8*, i8** %id_loc8, align 8
  %tmp = bitcast i8* %id_ptr6 to i32*
  %tmp10 = bitcast i8* %id_ptr9 to i32*
  %infix_operand = load i32, i32* %tmp, align 4
  %infix_operand11 = load i32, i32* %tmp10, align 4
  %tmp12 = add i32 %infix_operand, %infix_operand11
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp13 = bitcast i8* %malloccall to i32*
  store i32 %tmp12, i32* %tmp13, align 4
  %cell = bitcast i32* %tmp13 to i8*
  %tmp14 = bitcast i8* %cell to i32*
  %tmp15 = bitcast i8* %c to i32*
  %infix_operand16 = load i32, i32* %tmp14, align 4
  %infix_operand17 = load i32, i32* %tmp15, align 4
  %tmp18 = add i32 %infix_operand16, %infix_operand17
  %malloccall19 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp20 = bitcast i8* %malloccall19 to i32*
  store i32 %tmp18, i32* %tmp20, align 4
  %cell21 = bitcast i32* %tmp20 to i8*
  ret i8* %cell21
}

declare noalias i8* @malloc(i32)

