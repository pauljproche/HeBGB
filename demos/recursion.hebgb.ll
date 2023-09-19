; ModuleID = 'HeBGB'
source_filename = "HeBGB"

declare i32 @print_int(i32)

declare i32 @print_string(i8*)

define i32 @main() {
entry:
  %malloccall = tail call i8* @malloc(i32 ptrtoint ([2 x i8*]* getelementptr ([2 x i8*], [2 x i8*]* null, i32 1) to i32))
  %clo = bitcast i8* %malloccall to [2 x i8*]*
  %f_ptr_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo, i32 0, i32 0
  store i8* bitcast (i8* (i8*, i8*)* @fact to i8*), i8** %f_ptr_loc, align 8
  %clo_ptr = bitcast [2 x i8*]* %clo to i8*
  %var_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo, i32 0, i32 1
  store i8* %clo_ptr, i8** %var_loc, align 8
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp = bitcast i8* %malloccall1 to i32*
  store i32 10, i32* %tmp, align 4
  %cell = bitcast i32* %tmp to i8*
  %clo_ptr2 = bitcast i8* %clo_ptr to i8* (i8*, i8*)**
  %clo_val = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %clo_ptr2, align 8
  %f_result = call i8* %clo_val(i8* %cell, i8* %clo_ptr)
  %tmp3 = bitcast i8* %f_result to i32*
  %tmp4 = load i32, i32* %tmp3, align 4
  %print_int = call i32 @print_int(i32 %tmp4)
  %malloccall5 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp6 = bitcast i8* %malloccall5 to i32*
  store i32 0, i32* %tmp6, align 4
  %cell7 = bitcast i32* %tmp6 to i8*
  %tmp8 = bitcast i8* %cell7 to i32*
  %ret_code = load i32, i32* %tmp8, align 4
  ret i32 %ret_code
}

define i8* @fact(i8* %n, i8* %curr_clo) {
entry:
  %clo_arr_ptr = bitcast i8* %curr_clo to [2 x i8*]*
  %id_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo_arr_ptr, i32 0, i32 1
  %id_ptr = load i8*, i8** %id_loc, align 8
  %malloccall = tail call i8* @malloc(i32 ptrtoint ([2 x i8*]* getelementptr ([2 x i8*], [2 x i8*]* null, i32 1) to i32))
  %clo = bitcast i8* %malloccall to [2 x i8*]*
  %f_ptr_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo, i32 0, i32 0
  store i8* bitcast (i8* (i8*, i8*)* @do_recursive_call to i8*), i8** %f_ptr_loc, align 8
  %clo_ptr = bitcast [2 x i8*]* %clo to i8*
  %var_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo, i32 0, i32 1
  store i8* %id_ptr, i8** %var_loc, align 8
  %malloccall1 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp = bitcast i8* %malloccall1 to i32*
  store i32 0, i32* %tmp, align 4
  %cell = bitcast i32* %tmp to i8*
  %tmp2 = bitcast i8* %n to i32*
  %tmp3 = bitcast i8* %cell to i32*
  %infix_operand = load i32, i32* %tmp2, align 4
  %infix_operand4 = load i32, i32* %tmp3, align 4
  %tmp5 = icmp eq i32 %infix_operand, %infix_operand4
  %tmp6 = sext i1 %tmp5 to i32
  %malloccall7 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp8 = bitcast i8* %malloccall7 to i32*
  store i32 %tmp6, i32* %tmp8, align 4
  %cell9 = bitcast i32* %tmp8 to i8*
  %result = alloca i8*, align 8
  %tmp22 = bitcast i8* %cell9 to i32*
  %test_val = load i32, i32* %tmp22, align 4
  %test_bool_val = trunc i32 %test_val to i1
  br i1 %test_bool_val, label %do, label %else

merge:                                            ; preds = %else, %do
  %result23 = load i8*, i8** %result, align 8
  ret i8* %result23

do:                                               ; preds = %entry
  %malloccall10 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp11 = bitcast i8* %malloccall10 to i32*
  store i32 1, i32* %tmp11, align 4
  %cell12 = bitcast i32* %tmp11 to i8*
  store i8* %cell12, i8** %result, align 8
  br label %merge

else:                                             ; preds = %entry
  %clo_ptr13 = bitcast i8* %clo_ptr to i8* (i8*, i8*)**
  %clo_val = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %clo_ptr13, align 8
  %f_result = call i8* %clo_val(i8* %n, i8* %clo_ptr)
  %tmp14 = bitcast i8* %n to i32*
  %tmp15 = bitcast i8* %f_result to i32*
  %infix_operand16 = load i32, i32* %tmp14, align 4
  %infix_operand17 = load i32, i32* %tmp15, align 4
  %tmp18 = mul i32 %infix_operand16, %infix_operand17
  %malloccall19 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp20 = bitcast i8* %malloccall19 to i32*
  store i32 %tmp18, i32* %tmp20, align 4
  %cell21 = bitcast i32* %tmp20 to i8*
  store i8* %cell21, i8** %result, align 8
  br label %merge
}

define i8* @do_recursive_call(i8* %n, i8* %curr_clo) {
entry:
  %clo_arr_ptr = bitcast i8* %curr_clo to [2 x i8*]*
  %id_loc = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo_arr_ptr, i32 0, i32 1
  %id_ptr = load i8*, i8** %id_loc, align 8
  %malloccall = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp = bitcast i8* %malloccall to i32*
  store i32 1, i32* %tmp, align 4
  %cell = bitcast i32* %tmp to i8*
  %tmp1 = bitcast i8* %n to i32*
  %tmp2 = bitcast i8* %cell to i32*
  %infix_operand = load i32, i32* %tmp1, align 4
  %infix_operand3 = load i32, i32* %tmp2, align 4
  %tmp4 = sub i32 %infix_operand, %infix_operand3
  %malloccall5 = tail call i8* @malloc(i32 ptrtoint (i32* getelementptr (i32, i32* null, i32 1) to i32))
  %tmp6 = bitcast i8* %malloccall5 to i32*
  store i32 %tmp4, i32* %tmp6, align 4
  %cell7 = bitcast i32* %tmp6 to i8*
  %clo_arr_ptr8 = bitcast i8* %curr_clo to [2 x i8*]*
  %id_loc9 = getelementptr inbounds [2 x i8*], [2 x i8*]* %clo_arr_ptr8, i32 0, i32 1
  %id_ptr10 = load i8*, i8** %id_loc9, align 8
  %clo_ptr = bitcast i8* %id_ptr10 to i8* (i8*, i8*)**
  %clo_val = load i8* (i8*, i8*)*, i8* (i8*, i8*)** %clo_ptr, align 8
  %f_result = call i8* %clo_val(i8* %cell7, i8* %id_ptr10)
  ret i8* %f_result
}

declare noalias i8* @malloc(i32)

