; ModuleID = '/home/firefox/firefox/mozilla-unified/widget/LSBUtils.cpp'
source_filename = "/home/firefox/firefox/mozilla-unified/widget/LSBUtils.cpp"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%class.nsTSubstring = type { %"class.mozilla::detail::nsTStringRepr" }
%"class.mozilla::detail::nsTStringRepr" = type { i8*, i32, i16, i16 }
%"class.std::vector" = type { %"struct.std::_Vector_base" }
%"struct.std::_Vector_base" = type { %"struct.std::_Vector_base<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > >::_Vector_impl" }
%"struct.std::_Vector_base<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > >::_Vector_impl" = type { %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"* }
%"class.std::__cxx11::basic_string" = type { %"struct.std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Alloc_hider", i64, %union.anon }
%"struct.std::__cxx11::basic_string<char, std::char_traits<char>, std::allocator<char> >::_Alloc_hider" = type { i8* }
%union.anon = type { i64, [8 x i8] }
%"class.std::allocator.0" = type { i8 }
%"struct.base::LaunchOptions" = type { i8, %"class.std::map", %"class.std::vector.6", %"class.mozilla::UniquePtr" }
%"class.std::map" = type { %"class.std::_Rb_tree" }
%"class.std::_Rb_tree" = type { %"struct.std::_Rb_tree<std::__cxx11::basic_string<char>, std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> >, std::_Select1st<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, std::less<std::__cxx11::basic_string<char> >, std::allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >::_Rb_tree_impl" }
%"struct.std::_Rb_tree<std::__cxx11::basic_string<char>, std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> >, std::_Select1st<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, std::less<std::__cxx11::basic_string<char> >, std::allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >::_Rb_tree_impl" = type { %"struct.std::_Rb_tree_key_compare", %"struct.std::_Rb_tree_header" }
%"struct.std::_Rb_tree_key_compare" = type { %"struct.std::less" }
%"struct.std::less" = type { i8 }
%"struct.std::_Rb_tree_header" = type { %"struct.std::_Rb_tree_node_base", i64 }
%"struct.std::_Rb_tree_node_base" = type { i32, %"struct.std::_Rb_tree_node_base"*, %"struct.std::_Rb_tree_node_base"*, %"struct.std::_Rb_tree_node_base"* }
%"class.std::vector.6" = type { %"struct.std::_Vector_base.7" }
%"struct.std::_Vector_base.7" = type { %"struct.std::_Vector_base<std::pair<int, int>, std::allocator<std::pair<int, int> > >::_Vector_impl" }
%"struct.std::_Vector_base<std::pair<int, int>, std::allocator<std::pair<int, int> > >::_Vector_impl" = type { %"struct.std::pair"*, %"struct.std::pair"*, %"struct.std::pair"* }
%"struct.std::pair" = type { i32, i32 }
%"class.mozilla::UniquePtr" = type { %"struct.mozilla::Pair" }
%"struct.mozilla::Pair" = type { %"struct.mozilla::detail::PairHelper" }
%"struct.mozilla::detail::PairHelper" = type { %"struct.base::LaunchOptions::ForkDelegate"* }
%"struct.base::LaunchOptions::ForkDelegate" = type { i32 (...)** }
%"class.std::allocator" = type { i8 }
%"class.std::initializer_list" = type { %"class.std::__cxx11::basic_string"*, i64 }
%"class.mozilla::DefaultDelete" = type { i8 }
%struct._IO_FILE = type { i32, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, i8*, %struct._IO_marker*, %struct._IO_FILE*, i32, i32, i64, i16, i8, [1 x i8], i8*, i64, %struct._IO_codecvt*, %struct._IO_wide_data*, %struct._IO_FILE*, i8*, i64, i32, [20 x i8] }
%struct._IO_marker = type opaque
%struct._IO_codecvt = type opaque
%struct._IO_wide_data = type opaque
%"class.mozilla::Scoped" = type { %struct._IO_FILE* }
%"struct.std::_Rb_tree_node" = type { %"struct.std::_Rb_tree_node_base", %"struct.__gnu_cxx::__aligned_membuf" }
%"struct.__gnu_cxx::__aligned_membuf" = type { [64 x i8] }
%"struct.std::pair.11" = type { %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string" }
%"class.__gnu_cxx::new_allocator" = type { i8 }
%"class.__gnu_cxx::__normal_iterator" = type { %"struct.std::pair"* }
%"class.std::move_iterator" = type { %"struct.std::pair"* }
%"class.std::allocator.8" = type { i8 }
%"class.__gnu_cxx::new_allocator.9" = type { i8 }

$_ZN4base13LaunchOptionsD2Ev = comdat any

$_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev = comdat any

$_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEED2Ev = comdat any

$_ZNSt12_Vector_baseISt4pairIiiESaIS1_EED2Ev = comdat any

$_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EED2Ev = comdat any

$_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_eraseEPSt13_Rb_tree_nodeIS8_E = comdat any

$_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_drop_nodeEPSt13_Rb_tree_nodeIS8_E = comdat any

$_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_ED2Ev = comdat any

$_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev = comdat any

$_ZNSt12_Destroy_auxILb0EE9__destroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEEvT_S9_ = comdat any

$_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_range_initializeIPKS5_EEvT_SB_St20forward_iterator_tag = comdat any

$_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE11_M_allocateEm = comdat any

$_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8allocateERS6_m = comdat any

$_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE8allocateEmPKv = comdat any

$_ZNSt20__uninitialized_copyILb0EE13__uninit_copyIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS7_EET0_T_SC_SB_ = comdat any

$_ZNSt6vectorISt4pairIiiESaIS1_EE12emplace_backIJS1_EEEvDpOT_ = comdat any

$_ZNSt6vectorISt4pairIiiESaIS1_EE17_M_realloc_insertIJS1_EEEvN9__gnu_cxx17__normal_iteratorIPS1_S3_EEDpOT_ = comdat any

$_ZNKSt6vectorISt4pairIiiESaIS1_EE12_M_check_lenEmPKc = comdat any

$_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE11_M_allocateEm = comdat any

$_ZNSt16allocator_traitsISaISt4pairIiiEEE8allocateERS2_m = comdat any

$_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE8allocateEmPKv = comdat any

$_ZN7mozilla21ScopedCloseFileTraits7releaseEP8_IO_FILE = comdat any

@.str = private unnamed_addr constant [6 x i8] c"-idrc\00", align 1
@.str.1 = private unnamed_addr constant [2 x i8] c"r\00", align 1
@.str.2 = private unnamed_addr constant [85 x i8] c"Distributor ID:\09%255[^\0A]\0ADescription:\09%255[^\0A]\0ARelease:\09%255[^\0A]\0ACodename:\09%255[^\0A]\0A\00", align 1
@.str.3 = private unnamed_addr constant [21 x i8] c"/usr/bin/lsb_release\00", align 1
@.str.5 = private unnamed_addr constant [27 x i8] c"fatal: STL threw bad_alloc\00", align 1
@.str.6 = private unnamed_addr constant [26 x i8] c"vector::_M_realloc_insert\00", align 1

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define hidden zeroext i1 @_ZN7mozilla6widget3lsb13GetLSBReleaseER12nsTSubstringIcES4_S4_S4_(%class.nsTSubstring* dereferenceable(16), %class.nsTSubstring* dereferenceable(16), %class.nsTSubstring* dereferenceable(16), %class.nsTSubstring* dereferenceable(16)) local_unnamed_addr #0 !dbg !2908 {
  %5 = alloca [2 x i32], align 4
  %6 = alloca %"class.std::vector", align 8
  %7 = alloca [2 x %"class.std::__cxx11::basic_string"], align 8
  %8 = alloca %"class.std::allocator.0", align 1
  %9 = alloca %"class.std::allocator.0", align 1
  %10 = alloca %"struct.base::LaunchOptions", align 8
  %11 = alloca %"struct.std::pair", align 4
  %12 = alloca i32, align 4
  %13 = alloca [256 x i8], align 16
  %14 = alloca [256 x i8], align 16
  %15 = alloca [256 x i8], align 16
  %16 = alloca [256 x i8], align 16
  call void @llvm.dbg.value(metadata %class.nsTSubstring* %0, metadata !2915, metadata !DIExpression()), !dbg !3579
  call void @llvm.dbg.value(metadata %class.nsTSubstring* %1, metadata !2916, metadata !DIExpression()), !dbg !3580
  call void @llvm.dbg.value(metadata %class.nsTSubstring* %2, metadata !2917, metadata !DIExpression()), !dbg !3581
  call void @llvm.dbg.value(metadata %class.nsTSubstring* %3, metadata !2918, metadata !DIExpression()), !dbg !3582
  %17 = tail call i32 @access(i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.3, i64 0, i64 0), i32 4) #7, !dbg !3583
  %18 = icmp eq i32 %17, 0, !dbg !3585
  br i1 %18, label %19, label %85, !dbg !3586

; <label>:19:                                     ; preds = %4
  %20 = bitcast [2 x i32]* %5 to i8*, !dbg !3587
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %20) #8, !dbg !3587
  call void @llvm.dbg.declare(metadata [2 x i32]* %5, metadata !2919, metadata !DIExpression()), !dbg !3588
  %21 = getelementptr inbounds [2 x i32], [2 x i32]* %5, i64 0, i64 0, !dbg !3589
  %22 = call i32 @pipe(i32* nonnull %21) #7, !dbg !3591
  %23 = icmp eq i32 %22, -1, !dbg !3592
  br i1 %23, label %83, label %24, !dbg !3593

; <label>:24:                                     ; preds = %19
  %25 = bitcast %"class.std::vector"* %6 to i8*, !dbg !3594
  call void @llvm.lifetime.start.p0i8(i64 24, i8* nonnull %25) #8, !dbg !3594
  %26 = bitcast [2 x %"class.std::__cxx11::basic_string"]* %7 to i8*, !dbg !3595
  call void @llvm.lifetime.start.p0i8(i64 64, i8* nonnull %26) #8, !dbg !3595
  %27 = getelementptr inbounds [2 x %"class.std::__cxx11::basic_string"], [2 x %"class.std::__cxx11::basic_string"]* %7, i64 0, i64 0, !dbg !3595
  %28 = getelementptr inbounds %"class.std::allocator.0", %"class.std::allocator.0"* %8, i64 0, i32 0, !dbg !3596
  call void @llvm.lifetime.start.p0i8(i64 1, i8* nonnull %28) #8, !dbg !3596
  call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_(%"class.std::__cxx11::basic_string"* nonnull %27, i8* getelementptr inbounds ([21 x i8], [21 x i8]* @.str.3, i64 0, i64 0), %"class.std::allocator.0"* nonnull dereferenceable(1) %8) #9, !dbg !3596
  %29 = getelementptr inbounds [2 x %"class.std::__cxx11::basic_string"], [2 x %"class.std::__cxx11::basic_string"]* %7, i64 0, i64 1, !dbg !3595
  %30 = getelementptr inbounds %"class.std::allocator.0", %"class.std::allocator.0"* %9, i64 0, i32 0, !dbg !3597
  call void @llvm.lifetime.start.p0i8(i64 1, i8* nonnull %30) #8, !dbg !3597
  call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_(%"class.std::__cxx11::basic_string"* nonnull %29, i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str, i64 0, i64 0), %"class.std::allocator.0"* nonnull dereferenceable(1) %9) #9, !dbg !3597
  call void @llvm.dbg.value(metadata %"class.std::vector"* %6, metadata !2923, metadata !DIExpression(DW_OP_deref)), !dbg !3598
  call void @llvm.dbg.value(metadata %"class.std::vector"* %6, metadata !3599, metadata !DIExpression()) #8, !dbg !3605
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !3603, metadata !DIExpression()) #8, !dbg !3607
  call void @llvm.dbg.value(metadata %"class.std::vector"* %6, metadata !3608, metadata !DIExpression()) #8, !dbg !3613
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !3611, metadata !DIExpression()) #8, !dbg !3615
  call void @llvm.dbg.value(metadata %"class.std::vector"* %6, metadata !3616, metadata !DIExpression()) #8, !dbg !3621
  call void @llvm.dbg.value(metadata %"class.std::allocator"* undef, metadata !3619, metadata !DIExpression()) #8, !dbg !3623
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %25, i8 0, i64 24, i1 false) #8, !dbg !3624
  call void @llvm.dbg.value(metadata %"class.std::initializer_list"* undef, metadata !3602, metadata !DIExpression(DW_OP_deref)) #8, !dbg !3625
  call void @llvm.dbg.value(metadata %"class.std::initializer_list"* undef, metadata !3602, metadata !DIExpression(DW_OP_deref)) #8, !dbg !3625
  call void @llvm.dbg.value(metadata %"class.std::initializer_list"* undef, metadata !3626, metadata !DIExpression()) #8, !dbg !3630
  %31 = getelementptr inbounds [2 x %"class.std::__cxx11::basic_string"], [2 x %"class.std::__cxx11::basic_string"]* %7, i64 0, i64 2, !dbg !3633
  call void @_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_range_initializeIPKS5_EEvT_SB_St20forward_iterator_tag(%"class.std::vector"* nonnull %6, %"class.std::__cxx11::basic_string"* nonnull %27, %"class.std::__cxx11::basic_string"* nonnull %31) #7, !dbg !3634
  br label %32, !dbg !3595

; <label>:32:                                     ; preds = %32, %24
  %33 = phi %"class.std::__cxx11::basic_string"* [ %31, %24 ], [ %34, %32 ], !dbg !3595
  %34 = getelementptr inbounds %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string"* %33, i64 -1, !dbg !3595
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %34, metadata !3635, metadata !DIExpression()) #8, !dbg !3642
  call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_disposeEv(%"class.std::__cxx11::basic_string"* nonnull %34) #7, !dbg !3644
  %35 = icmp eq %"class.std::__cxx11::basic_string"* %34, %27, !dbg !3595
  br i1 %35, label %36, label %32, !dbg !3595

; <label>:36:                                     ; preds = %32
  call void @llvm.lifetime.end.p0i8(i64 1, i8* nonnull %30) #8, !dbg !3595
  call void @llvm.lifetime.end.p0i8(i64 1, i8* nonnull %28) #8, !dbg !3595
  call void @llvm.lifetime.end.p0i8(i64 64, i8* nonnull %26) #8, !dbg !3595
  %37 = getelementptr inbounds %"struct.base::LaunchOptions", %"struct.base::LaunchOptions"* %10, i64 0, i32 0, !dbg !3646
  call void @llvm.lifetime.start.p0i8(i64 88, i8* nonnull %37) #8, !dbg !3646
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3145, metadata !DIExpression(DW_OP_deref)), !dbg !3647
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3648, metadata !DIExpression()) #8, !dbg !3656
  store i8 0, i8* %37, align 8, !dbg !3658
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3659, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)) #8, !dbg !3663
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3665, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)) #8, !dbg !3669
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3671, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)) #8, !dbg !3675
  %38 = getelementptr inbounds %"struct.base::LaunchOptions", %"struct.base::LaunchOptions"* %10, i64 0, i32 1, i32 0, i32 0, i32 0, i32 0, i32 0, !dbg !3677
  %39 = getelementptr inbounds i8, i8* %38, i64 8, !dbg !3677
  call void @llvm.dbg.value(metadata i8* %39, metadata !3678, metadata !DIExpression()) #8, !dbg !3682
  %40 = bitcast i8* %39 to i32*, !dbg !3684
  store i32 0, i32* %40, align 8, !dbg !3686
  call void @llvm.dbg.value(metadata i8* %39, metadata !3687, metadata !DIExpression()) #8, !dbg !3690
  %41 = getelementptr inbounds i8, i8* %38, i64 16, !dbg !3692
  %42 = bitcast i8* %41 to %"struct.std::_Rb_tree_node_base"**, !dbg !3692
  store %"struct.std::_Rb_tree_node_base"* null, %"struct.std::_Rb_tree_node_base"** %42, align 8, !dbg !3693
  %43 = getelementptr inbounds i8, i8* %38, i64 24, !dbg !3694
  %44 = bitcast i8* %43 to i8**, !dbg !3695
  store i8* %39, i8** %44, align 8, !dbg !3695
  %45 = getelementptr inbounds i8, i8* %38, i64 32, !dbg !3696
  %46 = bitcast i8* %45 to i8**, !dbg !3697
  store i8* %39, i8** %46, align 8, !dbg !3697
  %47 = getelementptr inbounds i8, i8* %38, i64 40, !dbg !3698
  %48 = bitcast i8* %47 to i64*, !dbg !3698
  store i64 0, i64* %48, align 8, !dbg !3699
  %49 = getelementptr inbounds %"struct.base::LaunchOptions", %"struct.base::LaunchOptions"* %10, i64 0, i32 2, !dbg !3700
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3701, metadata !DIExpression(DW_OP_plus_uconst, 80, DW_OP_stack_value)) #8, !dbg !3706
  call void @llvm.dbg.value(metadata i8* null, metadata !3704, metadata !DIExpression()) #8, !dbg !3708
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3709, metadata !DIExpression(DW_OP_plus_uconst, 80, DW_OP_stack_value)) #8, !dbg !3723
  call void @llvm.dbg.value(metadata %"class.mozilla::DefaultDelete"* undef, metadata !3721, metadata !DIExpression()) #8, !dbg !3725
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3726, metadata !DIExpression(DW_OP_plus_uconst, 80, DW_OP_stack_value)) #8, !dbg !3735
  %50 = bitcast %"class.std::vector.6"* %49 to i8*, !dbg !3700
  call void @llvm.memset.p0i8.i64(i8* nonnull align 8 %50, i8 0, i64 32, i1 false) #8, !dbg !3737
  %51 = bitcast %"struct.std::pair"* %11 to i8*, !dbg !3738
  call void @llvm.lifetime.start.p0i8(i64 8, i8* nonnull %51) #8, !dbg !3738
  %52 = getelementptr inbounds [2 x i32], [2 x i32]* %5, i64 0, i64 1, !dbg !3739
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %11, metadata !3740, metadata !DIExpression()), !dbg !3752
  call void @llvm.dbg.value(metadata i32* %52, metadata !3750, metadata !DIExpression()), !dbg !3754
  %53 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %11, i64 0, i32 0, !dbg !3755
  %54 = load i32, i32* %52, align 4, !dbg !3756
  store i32 %54, i32* %53, align 4, !dbg !3755
  %55 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %11, i64 0, i32 1, !dbg !3757
  store i32 1, i32* %55, align 4, !dbg !3757
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %49, metadata !3758, metadata !DIExpression()) #8, !dbg !3763
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %11, metadata !3761, metadata !DIExpression()) #8, !dbg !3765
  call void @_ZNSt6vectorISt4pairIiiESaIS1_EE12emplace_backIJS1_EEEvDpOT_(%"class.std::vector.6"* nonnull %49, %"struct.std::pair"* nonnull dereferenceable(8) %11) #7, !dbg !3766
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %51) #8, !dbg !3767
  store i8 1, i8* %37, align 8, !dbg !3768
  %56 = bitcast i32* %12 to i8*, !dbg !3769
  call void @llvm.lifetime.start.p0i8(i64 4, i8* nonnull %56) #8, !dbg !3769
  call void @llvm.dbg.value(metadata %"class.std::vector"* %6, metadata !2923, metadata !DIExpression(DW_OP_deref)), !dbg !3598
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3145, metadata !DIExpression(DW_OP_deref)), !dbg !3647
  call void @llvm.dbg.value(metadata i32* %12, metadata !3499, metadata !DIExpression(DW_OP_deref)), !dbg !3770
  %57 = call zeroext i1 @_ZN4base9LaunchAppERKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS6_EERKNS_13LaunchOptionsEPi(%"class.std::vector"* nonnull dereferenceable(24) %6, %"struct.base::LaunchOptions"* nonnull dereferenceable(88) %10, i32* nonnull %12) #7, !dbg !3771
  %58 = load i32, i32* %52, align 4, !dbg !3772
  %59 = call i32 @close(i32 %58) #7, !dbg !3773
  %60 = load i32, i32* %21, align 4, !dbg !3774
  br i1 %57, label %63, label %61, !dbg !3777

; <label>:61:                                     ; preds = %36
  %62 = call i32 @close(i32 %60) #7, !dbg !3778
  br label %81, !dbg !3779

; <label>:63:                                     ; preds = %36
  %64 = call %struct._IO_FILE* @fdopen(i32 %60, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @.str.1, i64 0, i64 0)) #7, !dbg !3780
  call void @llvm.dbg.value(metadata %"class.mozilla::Scoped"* undef, metadata !3506, metadata !DIExpression(DW_OP_deref)), !dbg !3781
  call void @llvm.dbg.value(metadata %"class.mozilla::Scoped"* undef, metadata !3506, metadata !DIExpression(DW_OP_deref)), !dbg !3781
  %65 = icmp eq %struct._IO_FILE* %64, null, !dbg !3782
  br i1 %65, label %66, label %69, !dbg !3784

; <label>:66:                                     ; preds = %63
  %67 = load i32, i32* %21, align 4, !dbg !3785
  %68 = call i32 @close(i32 %67) #7, !dbg !3787
  br label %79, !dbg !3788

; <label>:69:                                     ; preds = %63
  %70 = getelementptr inbounds [256 x i8], [256 x i8]* %13, i64 0, i64 0, !dbg !3789
  call void @llvm.lifetime.start.p0i8(i64 256, i8* nonnull %70) #8, !dbg !3789
  call void @llvm.dbg.declare(metadata [256 x i8]* %13, metadata !3572, metadata !DIExpression()), !dbg !3790
  %71 = getelementptr inbounds [256 x i8], [256 x i8]* %14, i64 0, i64 0, !dbg !3789
  call void @llvm.lifetime.start.p0i8(i64 256, i8* nonnull %71) #8, !dbg !3789
  call void @llvm.dbg.declare(metadata [256 x i8]* %14, metadata !3576, metadata !DIExpression()), !dbg !3791
  %72 = getelementptr inbounds [256 x i8], [256 x i8]* %15, i64 0, i64 0, !dbg !3789
  call void @llvm.lifetime.start.p0i8(i64 256, i8* nonnull %72) #8, !dbg !3789
  call void @llvm.dbg.declare(metadata [256 x i8]* %15, metadata !3577, metadata !DIExpression()), !dbg !3792
  %73 = getelementptr inbounds [256 x i8], [256 x i8]* %16, i64 0, i64 0, !dbg !3789
  call void @llvm.lifetime.start.p0i8(i64 256, i8* nonnull %73) #8, !dbg !3789
  call void @llvm.dbg.declare(metadata [256 x i8]* %16, metadata !3578, metadata !DIExpression()), !dbg !3793
  call void @llvm.dbg.value(metadata %"class.mozilla::Scoped"* undef, metadata !3506, metadata !DIExpression(DW_OP_deref)), !dbg !3781
  %74 = call i32 (%struct._IO_FILE*, i8*, ...) @fscanf(%struct._IO_FILE* nonnull %64, i8* getelementptr inbounds ([85 x i8], [85 x i8]* @.str.2, i64 0, i64 0), i8* nonnull %70, i8* nonnull %71, i8* nonnull %72, i8* nonnull %73) #9, !dbg !3794
  %75 = icmp eq i32 %74, 4, !dbg !3796
  br i1 %75, label %76, label %77, !dbg !3797

; <label>:76:                                     ; preds = %69
  call void @_ZN12nsTSubstringIcE6AssignEPKcj(%class.nsTSubstring* nonnull %0, i8* nonnull %70, i32 -1) #7, !dbg !3798
  call void @_ZN12nsTSubstringIcE6AssignEPKcj(%class.nsTSubstring* nonnull %1, i8* nonnull %71, i32 -1) #7, !dbg !3799
  call void @_ZN12nsTSubstringIcE6AssignEPKcj(%class.nsTSubstring* nonnull %2, i8* nonnull %72, i32 -1) #7, !dbg !3800
  call void @_ZN12nsTSubstringIcE6AssignEPKcj(%class.nsTSubstring* nonnull %3, i8* nonnull %73, i32 -1) #7, !dbg !3801
  br label %77, !dbg !3802

; <label>:77:                                     ; preds = %69, %76
  %78 = phi i1 [ true, %76 ], [ false, %69 ], !dbg !3803
  call void @llvm.lifetime.end.p0i8(i64 256, i8* nonnull %73) #8, !dbg !3804
  call void @llvm.lifetime.end.p0i8(i64 256, i8* nonnull %72) #8, !dbg !3804
  call void @llvm.lifetime.end.p0i8(i64 256, i8* nonnull %71) #8, !dbg !3804
  call void @llvm.lifetime.end.p0i8(i64 256, i8* nonnull %70) #8, !dbg !3804
  br label %79

; <label>:79:                                     ; preds = %77, %66
  %80 = phi i1 [ %78, %77 ], [ false, %66 ], !dbg !3805
  call void @llvm.dbg.value(metadata %"class.mozilla::Scoped"* undef, metadata !3506, metadata !DIExpression(DW_OP_deref)), !dbg !3781
  call void @llvm.dbg.value(metadata %"class.mozilla::Scoped"* undef, metadata !3806, metadata !DIExpression()) #8, !dbg !3810
  call void @_ZN7mozilla21ScopedCloseFileTraits7releaseEP8_IO_FILE(%struct._IO_FILE* %64) #7, !dbg !3812
  br label %81

; <label>:81:                                     ; preds = %79, %61
  %82 = phi i1 [ %80, %79 ], [ false, %61 ], !dbg !3774
  call void @llvm.lifetime.end.p0i8(i64 4, i8* nonnull %56) #8, !dbg !3804
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %10, metadata !3145, metadata !DIExpression(DW_OP_deref)), !dbg !3647
  call void @_ZN4base13LaunchOptionsD2Ev(%"struct.base::LaunchOptions"* nonnull %10) #7, !dbg !3804
  call void @llvm.lifetime.end.p0i8(i64 88, i8* nonnull %37) #8, !dbg !3804
  call void @llvm.dbg.value(metadata %"class.std::vector"* %6, metadata !2923, metadata !DIExpression(DW_OP_deref)), !dbg !3598
  call void @_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev(%"class.std::vector"* nonnull %6) #7, !dbg !3804
  call void @llvm.lifetime.end.p0i8(i64 24, i8* nonnull %25) #8, !dbg !3804
  br label %83

; <label>:83:                                     ; preds = %19, %81
  %84 = phi i1 [ %82, %81 ], [ false, %19 ], !dbg !3814
  call void @llvm.lifetime.end.p0i8(i64 8, i8* nonnull %20) #8, !dbg !3804
  br label %85

; <label>:85:                                     ; preds = %4, %83
  %86 = phi i1 [ %84, %83 ], [ false, %4 ], !dbg !3816
  ret i1 %86, !dbg !3804
}

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.declare(metadata, metadata, metadata) #1

; Function Attrs: minsize nounwind optsize
declare i32 @access(i8* nocapture readonly, i32) local_unnamed_addr #2

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.start.p0i8(i64, i8* nocapture) #3

; Function Attrs: minsize nounwind optsize
declare i32 @pipe(i32*) local_unnamed_addr #2

; Function Attrs: minsize nounwind optsize sspstrong uwtable
declare void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2EPKcRKS3_(%"class.std::__cxx11::basic_string"*, i8*, %"class.std::allocator.0"* dereferenceable(1)) unnamed_addr #0 align 2

; Function Attrs: argmemonly nounwind
declare void @llvm.lifetime.end.p0i8(i64, i8* nocapture) #3

; Function Attrs: minsize optsize
declare hidden zeroext i1 @_ZN4base9LaunchAppERKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS6_EERKNS_13LaunchOptionsEPi(%"class.std::vector"* dereferenceable(24), %"struct.base::LaunchOptions"* dereferenceable(88), i32*) local_unnamed_addr #4

; Function Attrs: minsize optsize
declare i32 @close(i32) local_unnamed_addr #4

; Function Attrs: minsize nounwind optsize
declare noalias %struct._IO_FILE* @fdopen(i32, i8* nocapture readonly) local_unnamed_addr #2

; Function Attrs: minsize nounwind optsize
declare i32 @fscanf(%struct._IO_FILE* nocapture, i8* nocapture readonly, ...) local_unnamed_addr #2

; Function Attrs: minsize optsize
declare hidden void @_ZN12nsTSubstringIcE6AssignEPKcj(%class.nsTSubstring*, i8*, i32) local_unnamed_addr #4

; Function Attrs: inlinehint minsize nounwind optsize sspstrong uwtable
define linkonce_odr hidden void @_ZN4base13LaunchOptionsD2Ev(%"struct.base::LaunchOptions"*) unnamed_addr #5 comdat align 2 !dbg !3818 {
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %0, metadata !3821, metadata !DIExpression()), !dbg !3822
  %2 = getelementptr inbounds %"struct.base::LaunchOptions", %"struct.base::LaunchOptions"* %0, i64 0, i32 3, !dbg !3823
  tail call void @_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEED2Ev(%"class.mozilla::UniquePtr"* nonnull %2) #7, !dbg !3823
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %0, metadata !3825, metadata !DIExpression(DW_OP_plus_uconst, 56, DW_OP_stack_value)) #8, !dbg !3828
  %3 = getelementptr inbounds %"struct.base::LaunchOptions", %"struct.base::LaunchOptions"* %0, i64 0, i32 2, i32 0, !dbg !3830
  tail call void @_ZNSt12_Vector_baseISt4pairIiiESaIS1_EED2Ev(%"struct.std::_Vector_base.7"* nonnull %3) #7, !dbg !3832
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions"* %0, metadata !3833, metadata !DIExpression(DW_OP_plus_uconst, 8, DW_OP_stack_value)) #8, !dbg !3836
  %4 = getelementptr inbounds %"struct.base::LaunchOptions", %"struct.base::LaunchOptions"* %0, i64 0, i32 1, i32 0, !dbg !3838
  tail call void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EED2Ev(%"class.std::_Rb_tree"* nonnull %4) #7, !dbg !3838
  ret void, !dbg !3840
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev(%"class.std::vector"*) unnamed_addr #0 comdat align 2 !dbg !3841 {
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !3843, metadata !DIExpression()), !dbg !3844
  %2 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, !dbg !3845
  %3 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 0, !dbg !3847
  %4 = load %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"** %3, align 8, !dbg !3847
  %5 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 1, !dbg !3848
  %6 = load %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"** %5, align 8, !dbg !3848
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %4, metadata !3849, metadata !DIExpression()) #8, !dbg !3859
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !3855, metadata !DIExpression()) #8, !dbg !3861
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base"* %2, metadata !3856, metadata !DIExpression()) #8, !dbg !3862
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %4, metadata !3863, metadata !DIExpression()) #8, !dbg !3870
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !3868, metadata !DIExpression()) #8, !dbg !3872
  tail call void @_ZNSt12_Destroy_auxILb0EE9__destroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEEvT_S9_(%"class.std::__cxx11::basic_string"* %4, %"class.std::__cxx11::basic_string"* %6) #7, !dbg !3873
  tail call void @_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev(%"struct.std::_Vector_base"* %2) #7, !dbg !3874
  ret void, !dbg !3875
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr hidden void @_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEED2Ev(%"class.mozilla::UniquePtr"*) unnamed_addr #0 comdat align 2 !dbg !3876 {
  call void @llvm.dbg.value(metadata %"class.mozilla::UniquePtr"* %0, metadata !3878, metadata !DIExpression()), !dbg !3879
  call void @llvm.dbg.value(metadata %"class.mozilla::UniquePtr"* %0, metadata !3880, metadata !DIExpression()) #8, !dbg !3885
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions::ForkDelegate"* null, metadata !3883, metadata !DIExpression()) #8, !dbg !3888
  call void @llvm.dbg.value(metadata %"class.mozilla::UniquePtr"* %0, metadata !3889, metadata !DIExpression()) #8, !dbg !3892
  call void @llvm.dbg.value(metadata %"class.mozilla::UniquePtr"* %0, metadata !3894, metadata !DIExpression()) #8, !dbg !3897
  %2 = getelementptr inbounds %"class.mozilla::UniquePtr", %"class.mozilla::UniquePtr"* %0, i64 0, i32 0, i32 0, i32 0, !dbg !3899
  %3 = load %"struct.base::LaunchOptions::ForkDelegate"*, %"struct.base::LaunchOptions::ForkDelegate"** %2, align 8, !dbg !3900
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions::ForkDelegate"* %3, metadata !3884, metadata !DIExpression()) #8, !dbg !3901
  call void @llvm.dbg.value(metadata %"class.mozilla::UniquePtr"* %0, metadata !3889, metadata !DIExpression()) #8, !dbg !3902
  call void @llvm.dbg.value(metadata %"class.mozilla::UniquePtr"* %0, metadata !3894, metadata !DIExpression()) #8, !dbg !3904
  store %"struct.base::LaunchOptions::ForkDelegate"* null, %"struct.base::LaunchOptions::ForkDelegate"** %2, align 8, !dbg !3906
  %4 = icmp eq %"struct.base::LaunchOptions::ForkDelegate"* %3, null, !dbg !3907
  br i1 %4, label %10, label %5, !dbg !3909

; <label>:5:                                      ; preds = %1
  call void @llvm.dbg.value(metadata %"struct.base::LaunchOptions::ForkDelegate"* %3, metadata !3910, metadata !DIExpression()) #8, !dbg !3915
  %6 = bitcast %"struct.base::LaunchOptions::ForkDelegate"* %3 to void (%"struct.base::LaunchOptions::ForkDelegate"*)***, !dbg !3918
  %7 = load void (%"struct.base::LaunchOptions::ForkDelegate"*)**, void (%"struct.base::LaunchOptions::ForkDelegate"*)*** %6, align 8, !dbg !3918
  %8 = getelementptr inbounds void (%"struct.base::LaunchOptions::ForkDelegate"*)*, void (%"struct.base::LaunchOptions::ForkDelegate"*)** %7, i64 1, !dbg !3918
  %9 = load void (%"struct.base::LaunchOptions::ForkDelegate"*)*, void (%"struct.base::LaunchOptions::ForkDelegate"*)** %8, align 8, !dbg !3918
  tail call void %9(%"struct.base::LaunchOptions::ForkDelegate"* nonnull %3) #7, !dbg !3918
  br label %10, !dbg !3919

; <label>:10:                                     ; preds = %1, %5
  ret void, !dbg !3920
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt12_Vector_baseISt4pairIiiESaIS1_EED2Ev(%"struct.std::_Vector_base.7"*) unnamed_addr #0 comdat align 2 !dbg !3921 {
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %0, metadata !3923, metadata !DIExpression()), !dbg !3925
  %2 = getelementptr inbounds %"struct.std::_Vector_base.7", %"struct.std::_Vector_base.7"* %0, i64 0, i32 0, i32 0, !dbg !3926
  %3 = load %"struct.std::pair"*, %"struct.std::pair"** %2, align 8, !dbg !3926
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %0, metadata !3928, metadata !DIExpression()) #8, !dbg !3933
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %3, metadata !3931, metadata !DIExpression()) #8, !dbg !3935
  %4 = icmp eq %"struct.std::pair"* %3, null, !dbg !3936
  br i1 %4, label %7, label %5, !dbg !3938

; <label>:5:                                      ; preds = %1
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %0, metadata !3939, metadata !DIExpression()) #8, !dbg !3944
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %3, metadata !3942, metadata !DIExpression()) #8, !dbg !3946
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %0, metadata !3947, metadata !DIExpression()) #8, !dbg !3953
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %3, metadata !3950, metadata !DIExpression()) #8, !dbg !3955
  %6 = bitcast %"struct.std::pair"* %3 to i8*, !dbg !3956
  call void @llvm.dbg.value(metadata i8* %6, metadata !3957, metadata !DIExpression()) #8, !dbg !3961
  tail call void @free(i8* %6) #7, !dbg !3963
  br label %7, !dbg !3964

; <label>:7:                                      ; preds = %1, %5
  ret void, !dbg !3965
}

; Function Attrs: minsize nounwind optsize
declare void @free(i8* nocapture) local_unnamed_addr #2

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EED2Ev(%"class.std::_Rb_tree"*) unnamed_addr #0 comdat align 2 !dbg !3966 {
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !3968, metadata !DIExpression()), !dbg !3969
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !3970, metadata !DIExpression()), !dbg !3973
  %2 = getelementptr inbounds %"class.std::_Rb_tree", %"class.std::_Rb_tree"* %0, i64 0, i32 0, i32 0, i32 0, i32 0, !dbg !3976
  %3 = getelementptr inbounds i8, i8* %2, i64 16, !dbg !3977
  %4 = bitcast i8* %3 to %"struct.std::_Rb_tree_node"**, !dbg !3977
  %5 = load %"struct.std::_Rb_tree_node"*, %"struct.std::_Rb_tree_node"** %4, align 8, !dbg !3977
  tail call void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_eraseEPSt13_Rb_tree_nodeIS8_E(%"class.std::_Rb_tree"* %0, %"struct.std::_Rb_tree_node"* %5) #9, !dbg !3978
  ret void, !dbg !3979
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_eraseEPSt13_Rb_tree_nodeIS8_E(%"class.std::_Rb_tree"*, %"struct.std::_Rb_tree_node"*) local_unnamed_addr #0 comdat align 2 !dbg !3980 {
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !3982, metadata !DIExpression()), !dbg !3986
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !3983, metadata !DIExpression()), !dbg !3987
  br label %3, !dbg !3988

; <label>:3:                                      ; preds = %6, %2
  %4 = phi %"struct.std::_Rb_tree_node"* [ %1, %2 ], [ %12, %6 ]
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %4, metadata !3983, metadata !DIExpression()), !dbg !3987
  %5 = icmp eq %"struct.std::_Rb_tree_node"* %4, null, !dbg !3989
  br i1 %5, label %13, label %6, !dbg !3988

; <label>:6:                                      ; preds = %3
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %4, metadata !3990, metadata !DIExpression()), !dbg !3993
  %7 = getelementptr inbounds %"struct.std::_Rb_tree_node", %"struct.std::_Rb_tree_node"* %4, i64 0, i32 0, i32 3, !dbg !3995
  %8 = bitcast %"struct.std::_Rb_tree_node_base"** %7 to %"struct.std::_Rb_tree_node"**, !dbg !3995
  %9 = load %"struct.std::_Rb_tree_node"*, %"struct.std::_Rb_tree_node"** %8, align 8, !dbg !3995
  tail call void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_eraseEPSt13_Rb_tree_nodeIS8_E(%"class.std::_Rb_tree"* %0, %"struct.std::_Rb_tree_node"* %9) #9, !dbg !3996
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %4, metadata !3997, metadata !DIExpression()), !dbg !4000
  %10 = getelementptr inbounds %"struct.std::_Rb_tree_node", %"struct.std::_Rb_tree_node"* %4, i64 0, i32 0, i32 2, !dbg !4002
  %11 = bitcast %"struct.std::_Rb_tree_node_base"** %10 to %"struct.std::_Rb_tree_node"**, !dbg !4002
  %12 = load %"struct.std::_Rb_tree_node"*, %"struct.std::_Rb_tree_node"** %11, align 8, !dbg !4002
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %12, metadata !3984, metadata !DIExpression()), !dbg !4003
  tail call void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_drop_nodeEPSt13_Rb_tree_nodeIS8_E(%"class.std::_Rb_tree"* %0, %"struct.std::_Rb_tree_node"* nonnull %4) #7, !dbg !4004
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %12, metadata !3983, metadata !DIExpression()), !dbg !3987
  br label %3, !dbg !3988, !llvm.loop !4005

; <label>:13:                                     ; preds = %3
  ret void, !dbg !4007
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_drop_nodeEPSt13_Rb_tree_nodeIS8_E(%"class.std::_Rb_tree"*, %"struct.std::_Rb_tree_node"*) local_unnamed_addr #0 comdat align 2 !dbg !4008 {
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4010, metadata !DIExpression()), !dbg !4012
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !4011, metadata !DIExpression()), !dbg !4013
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4014, metadata !DIExpression()) #8, !dbg !4018
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !4017, metadata !DIExpression()) #8, !dbg !4020
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !4021, metadata !DIExpression()) #8, !dbg !4024
  %3 = getelementptr inbounds %"struct.std::_Rb_tree_node", %"struct.std::_Rb_tree_node"* %1, i64 0, i32 1, !dbg !4026
  call void @llvm.dbg.value(metadata %"struct.__gnu_cxx::__aligned_membuf"* %3, metadata !4027, metadata !DIExpression()) #8, !dbg !4031
  %4 = bitcast %"struct.__gnu_cxx::__aligned_membuf"* %3 to %"struct.std::pair.11"*, !dbg !4033
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4034, metadata !DIExpression()) #8, !dbg !4068
  call void @llvm.dbg.value(metadata %"struct.std::pair.11"* %4, metadata !4067, metadata !DIExpression()) #8, !dbg !4070
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4071, metadata !DIExpression()) #8, !dbg !4079
  call void @llvm.dbg.value(metadata %"struct.std::pair.11"* %4, metadata !4077, metadata !DIExpression()) #8, !dbg !4081
  tail call void @_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_ED2Ev(%"struct.std::pair.11"* nonnull %4) #7, !dbg !4082
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4083, metadata !DIExpression()) #8, !dbg !4087
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !4086, metadata !DIExpression()) #8, !dbg !4089
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4090, metadata !DIExpression()) #8, !dbg !4095
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !4093, metadata !DIExpression()) #8, !dbg !4097
  call void @llvm.dbg.value(metadata i64 1, metadata !4094, metadata !DIExpression()) #8, !dbg !4098
  call void @llvm.dbg.value(metadata %"class.std::_Rb_tree"* %0, metadata !4099, metadata !DIExpression()) #8, !dbg !4104
  call void @llvm.dbg.value(metadata %"struct.std::_Rb_tree_node"* %1, metadata !4102, metadata !DIExpression()) #8, !dbg !4106
  call void @llvm.dbg.value(metadata i64 1, metadata !4103, metadata !DIExpression()) #8, !dbg !4107
  %5 = bitcast %"struct.std::_Rb_tree_node"* %1 to i8*, !dbg !4108
  call void @llvm.dbg.value(metadata i8* %5, metadata !3957, metadata !DIExpression()) #8, !dbg !4109
  tail call void @free(i8* %5) #7, !dbg !4111
  ret void, !dbg !4112
}

; Function Attrs: inlinehint minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_ED2Ev(%"struct.std::pair.11"*) unnamed_addr #5 comdat align 2 !dbg !4113 {
  call void @llvm.dbg.value(metadata %"struct.std::pair.11"* %0, metadata !4118, metadata !DIExpression()), !dbg !4119
  %2 = getelementptr inbounds %"struct.std::pair.11", %"struct.std::pair.11"* %0, i64 0, i32 1, !dbg !4120
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !3635, metadata !DIExpression()) #8, !dbg !4122
  tail call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_disposeEv(%"class.std::__cxx11::basic_string"* nonnull %2) #7, !dbg !4124
  %3 = getelementptr inbounds %"struct.std::pair.11", %"struct.std::pair.11"* %0, i64 0, i32 0, !dbg !4120
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %3, metadata !3635, metadata !DIExpression()) #8, !dbg !4125
  tail call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_disposeEv(%"class.std::__cxx11::basic_string"* %3) #7, !dbg !4127
  ret void, !dbg !4128
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
declare void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_disposeEv(%"class.std::__cxx11::basic_string"*) local_unnamed_addr #0 align 2

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev(%"struct.std::_Vector_base"*) unnamed_addr #0 comdat align 2 !dbg !4129 {
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base"* %0, metadata !4131, metadata !DIExpression()), !dbg !4132
  %2 = getelementptr inbounds %"struct.std::_Vector_base", %"struct.std::_Vector_base"* %0, i64 0, i32 0, i32 0, !dbg !4133
  %3 = load %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"** %2, align 8, !dbg !4133
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base"* %0, metadata !4135, metadata !DIExpression()) #8, !dbg !4140
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %3, metadata !4138, metadata !DIExpression()) #8, !dbg !4142
  %4 = icmp eq %"class.std::__cxx11::basic_string"* %3, null, !dbg !4143
  br i1 %4, label %7, label %5, !dbg !4145

; <label>:5:                                      ; preds = %1
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base"* %0, metadata !4146, metadata !DIExpression()) #8, !dbg !4151
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %3, metadata !4149, metadata !DIExpression()) #8, !dbg !4153
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base"* %0, metadata !4154, metadata !DIExpression()) #8, !dbg !4160
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %3, metadata !4157, metadata !DIExpression()) #8, !dbg !4162
  %6 = bitcast %"class.std::__cxx11::basic_string"* %3 to i8*, !dbg !4163
  call void @llvm.dbg.value(metadata i8* %6, metadata !3957, metadata !DIExpression()) #8, !dbg !4164
  tail call void @free(i8* %6) #7, !dbg !4166
  br label %7, !dbg !4167

; <label>:7:                                      ; preds = %1, %5
  ret void, !dbg !4168
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt12_Destroy_auxILb0EE9__destroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEEvT_S9_(%"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"*) local_unnamed_addr #0 comdat align 2 !dbg !4169 {
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %0, metadata !4174, metadata !DIExpression()), !dbg !4176
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4175, metadata !DIExpression()), !dbg !4177
  br label %3, !dbg !4178

; <label>:3:                                      ; preds = %6, %2
  %4 = phi %"class.std::__cxx11::basic_string"* [ %0, %2 ], [ %7, %6 ]
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %4, metadata !4174, metadata !DIExpression()), !dbg !4176
  %5 = icmp eq %"class.std::__cxx11::basic_string"* %4, %1, !dbg !4179
  br i1 %5, label %8, label %6, !dbg !4182

; <label>:6:                                      ; preds = %3
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %4, metadata !4183, metadata !DIExpression()) #8, !dbg !4188
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %4, metadata !3635, metadata !DIExpression()) #8, !dbg !4190
  tail call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEE10_M_disposeEv(%"class.std::__cxx11::basic_string"* %4) #7, !dbg !4192
  %7 = getelementptr inbounds %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string"* %4, i64 1, !dbg !4193
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %7, metadata !4174, metadata !DIExpression()), !dbg !4176
  br label %3, !dbg !4194, !llvm.loop !4195

; <label>:8:                                      ; preds = %3
  ret void, !dbg !4197
}

; Function Attrs: minsize noreturn optsize
declare extern_weak void @mozalloc_abort(i8*) local_unnamed_addr #6

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_range_initializeIPKS5_EEvT_SB_St20forward_iterator_tag(%"class.std::vector"*, %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"*) local_unnamed_addr #0 comdat align 2 !dbg !4198 {
  call void @llvm.dbg.value(metadata %"class.std::vector"* %0, metadata !4209, metadata !DIExpression()), !dbg !4215
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4210, metadata !DIExpression()), !dbg !4216
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4211, metadata !DIExpression()), !dbg !4217
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4218, metadata !DIExpression()), !dbg !4231
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4228, metadata !DIExpression()), !dbg !4233
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4234, metadata !DIExpression()), !dbg !4249
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4245, metadata !DIExpression()), !dbg !4251
  %4 = ptrtoint %"class.std::__cxx11::basic_string"* %2 to i64, !dbg !4252
  %5 = ptrtoint %"class.std::__cxx11::basic_string"* %1 to i64, !dbg !4252
  %6 = sub i64 %4, %5, !dbg !4252
  %7 = ashr exact i64 %6, 5, !dbg !4252
  call void @llvm.dbg.value(metadata i64 %7, metadata !4213, metadata !DIExpression()), !dbg !4253
  %8 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, !dbg !4254
  %9 = tail call %"class.std::__cxx11::basic_string"* @_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE11_M_allocateEm(%"struct.std::_Vector_base"* %8, i64 %7) #9, !dbg !4254
  %10 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 0, !dbg !4255
  store %"class.std::__cxx11::basic_string"* %9, %"class.std::__cxx11::basic_string"** %10, align 8, !dbg !4256
  %11 = getelementptr inbounds %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string"* %9, i64 %7, !dbg !4257
  %12 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 2, !dbg !4258
  store %"class.std::__cxx11::basic_string"* %11, %"class.std::__cxx11::basic_string"** %12, align 8, !dbg !4259
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4260, metadata !DIExpression()) #8, !dbg !4270
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4266, metadata !DIExpression()) #8, !dbg !4272
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %9, metadata !4267, metadata !DIExpression()) #8, !dbg !4273
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4274, metadata !DIExpression()) #8, !dbg !4283
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4279, metadata !DIExpression()) #8, !dbg !4285
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %9, metadata !4280, metadata !DIExpression()) #8, !dbg !4286
  call void @llvm.dbg.value(metadata i8 1, metadata !4281, metadata !DIExpression()) #8, !dbg !4287
  %13 = tail call %"class.std::__cxx11::basic_string"* @_ZNSt20__uninitialized_copyILb0EE13__uninit_copyIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS7_EET0_T_SC_SB_(%"class.std::__cxx11::basic_string"* %1, %"class.std::__cxx11::basic_string"* %2, %"class.std::__cxx11::basic_string"* %9) #7, !dbg !4288
  %14 = getelementptr inbounds %"class.std::vector", %"class.std::vector"* %0, i64 0, i32 0, i32 0, i32 1, !dbg !4289
  store %"class.std::__cxx11::basic_string"* %13, %"class.std::__cxx11::basic_string"** %14, align 8, !dbg !4290
  ret void, !dbg !4291
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"class.std::__cxx11::basic_string"* @_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE11_M_allocateEm(%"struct.std::_Vector_base"*, i64) local_unnamed_addr #0 comdat align 2 !dbg !4292 {
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base"* %0, metadata !4294, metadata !DIExpression()), !dbg !4296
  call void @llvm.dbg.value(metadata i64 %1, metadata !4295, metadata !DIExpression()), !dbg !4297
  %3 = icmp eq i64 %1, 0, !dbg !4298
  br i1 %3, label %7, label %4, !dbg !4299

; <label>:4:                                      ; preds = %2
  %5 = bitcast %"struct.std::_Vector_base"* %0 to %"class.std::allocator"*, !dbg !4300
  %6 = tail call %"class.std::__cxx11::basic_string"* @_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8allocateERS6_m(%"class.std::allocator"* dereferenceable(1) %5, i64 %1) #9, !dbg !4301
  br label %7, !dbg !4299

; <label>:7:                                      ; preds = %2, %4
  %8 = phi %"class.std::__cxx11::basic_string"* [ %6, %4 ], [ null, %2 ], !dbg !4299
  ret %"class.std::__cxx11::basic_string"* %8, !dbg !4302
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"class.std::__cxx11::basic_string"* @_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8allocateERS6_m(%"class.std::allocator"* dereferenceable(1), i64) local_unnamed_addr #0 comdat align 2 !dbg !4303 {
  call void @llvm.dbg.value(metadata %"class.std::allocator"* %0, metadata !4305, metadata !DIExpression()), !dbg !4307
  call void @llvm.dbg.value(metadata i64 %1, metadata !4306, metadata !DIExpression()), !dbg !4308
  %3 = bitcast %"class.std::allocator"* %0 to %"class.__gnu_cxx::new_allocator"*, !dbg !4309
  %4 = tail call %"class.std::__cxx11::basic_string"* @_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE8allocateEmPKv(%"class.__gnu_cxx::new_allocator"* nonnull %3, i64 %1, i8* null) #9, !dbg !4310
  ret %"class.std::__cxx11::basic_string"* %4, !dbg !4311
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"class.std::__cxx11::basic_string"* @_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE8allocateEmPKv(%"class.__gnu_cxx::new_allocator"*, i64, i8*) local_unnamed_addr #0 comdat align 2 !dbg !4312 {
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::new_allocator"* %0, metadata !4314, metadata !DIExpression()), !dbg !4317
  call void @llvm.dbg.value(metadata i64 %1, metadata !4315, metadata !DIExpression()), !dbg !4318
  call void @llvm.dbg.value(metadata i8* %2, metadata !4316, metadata !DIExpression()), !dbg !4319
  %4 = icmp ugt i64 %1, 576460752303423487, !dbg !4320
  br i1 %4, label %5, label %6, !dbg !4322

; <label>:5:                                      ; preds = %3
  tail call void @mozalloc_abort(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str.5, i64 0, i64 0)) #10, !dbg !4323
  unreachable, !dbg !4323

; <label>:6:                                      ; preds = %3
  %7 = shl i64 %1, 5, !dbg !4327
  call void @llvm.dbg.value(metadata i64 %7, metadata !4328, metadata !DIExpression()) #8, !dbg !4331
  %8 = tail call noalias i8* @moz_xmalloc(i64 %7) #7, !dbg !4333
  %9 = bitcast i8* %8 to %"class.std::__cxx11::basic_string"*, !dbg !4334
  ret %"class.std::__cxx11::basic_string"* %9, !dbg !4335
}

; Function Attrs: minsize optsize
declare extern_weak noalias i8* @moz_xmalloc(i64) local_unnamed_addr #4

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"class.std::__cxx11::basic_string"* @_ZNSt20__uninitialized_copyILb0EE13__uninit_copyIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS7_EET0_T_SC_SB_(%"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"*) local_unnamed_addr #0 comdat align 2 !dbg !4336 {
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %0, metadata !4342, metadata !DIExpression()), !dbg !4346
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %1, metadata !4343, metadata !DIExpression()), !dbg !4347
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4344, metadata !DIExpression()), !dbg !4348
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %2, metadata !4345, metadata !DIExpression()), !dbg !4349
  br label %4, !dbg !4350

; <label>:4:                                      ; preds = %8, %3
  %5 = phi %"class.std::__cxx11::basic_string"* [ %0, %3 ], [ %9, %8 ]
  %6 = phi %"class.std::__cxx11::basic_string"* [ %2, %3 ], [ %10, %8 ], !dbg !4353
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %5, metadata !4342, metadata !DIExpression()), !dbg !4346
  %7 = icmp eq %"class.std::__cxx11::basic_string"* %5, %1, !dbg !4356
  br i1 %7, label %11, label %8, !dbg !4357

; <label>:8:                                      ; preds = %4
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4358, metadata !DIExpression()) #8, !dbg !4369
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %5, metadata !4363, metadata !DIExpression()) #8, !dbg !4371
  tail call void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2ERKS4_(%"class.std::__cxx11::basic_string"* %6, %"class.std::__cxx11::basic_string"* nonnull dereferenceable(32) %5) #7, !dbg !4372
  %9 = getelementptr inbounds %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string"* %5, i64 1, !dbg !4373
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %9, metadata !4342, metadata !DIExpression()), !dbg !4346
  %10 = getelementptr inbounds %"class.std::__cxx11::basic_string", %"class.std::__cxx11::basic_string"* %6, i64 1, !dbg !4374
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %10, metadata !4345, metadata !DIExpression()), !dbg !4349
  br label %4, !dbg !4375, !llvm.loop !4376

; <label>:11:                                     ; preds = %4
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  call void @llvm.dbg.value(metadata %"class.std::__cxx11::basic_string"* %6, metadata !4345, metadata !DIExpression()), !dbg !4349
  ret %"class.std::__cxx11::basic_string"* %6, !dbg !4378
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
declare void @_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEC2ERKS4_(%"class.std::__cxx11::basic_string"*, %"class.std::__cxx11::basic_string"* dereferenceable(32)) unnamed_addr #0 align 2

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt6vectorISt4pairIiiESaIS1_EE12emplace_backIJS1_EEEvDpOT_(%"class.std::vector.6"*, %"struct.std::pair"* dereferenceable(8)) local_unnamed_addr #0 comdat align 2 !dbg !4379 {
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4388, metadata !DIExpression()), !dbg !4390
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4389, metadata !DIExpression()), !dbg !4391
  %3 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, i32 0, i32 1, !dbg !4392
  %4 = load %"struct.std::pair"*, %"struct.std::pair"** %3, align 8, !dbg !4392
  %5 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, i32 0, i32 2, !dbg !4394
  %6 = load %"struct.std::pair"*, %"struct.std::pair"** %5, align 8, !dbg !4394
  %7 = icmp eq %"struct.std::pair"* %4, %6, !dbg !4395
  br i1 %7, label %14, label %8, !dbg !4396

; <label>:8:                                      ; preds = %2
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4397, metadata !DIExpression()), !dbg !4407
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %4, metadata !4405, metadata !DIExpression()), !dbg !4410
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4406, metadata !DIExpression()), !dbg !4411
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4412, metadata !DIExpression()), !dbg !4420
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %4, metadata !4418, metadata !DIExpression()), !dbg !4422
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4419, metadata !DIExpression()), !dbg !4423
  %9 = bitcast %"struct.std::pair"* %1 to i64*, !dbg !4424
  %10 = bitcast %"struct.std::pair"* %4 to i64*, !dbg !4424
  %11 = load i64, i64* %9, align 4, !dbg !4424
  store i64 %11, i64* %10, align 4, !dbg !4424
  %12 = load %"struct.std::pair"*, %"struct.std::pair"** %3, align 8, !dbg !4425
  %13 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %12, i64 1, !dbg !4425
  store %"struct.std::pair"* %13, %"struct.std::pair"** %3, align 8, !dbg !4425
  br label %15, !dbg !4426

; <label>:14:                                     ; preds = %2
  tail call void @_ZNSt6vectorISt4pairIiiESaIS1_EE17_M_realloc_insertIJS1_EEEvN9__gnu_cxx17__normal_iteratorIPS1_S3_EEDpOT_(%"class.std::vector.6"* nonnull %0, %"struct.std::pair"* %4, %"struct.std::pair"* nonnull dereferenceable(8) %1) #9, !dbg !4427
  br label %15

; <label>:15:                                     ; preds = %14, %8
  ret void, !dbg !4428
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr void @_ZNSt6vectorISt4pairIiiESaIS1_EE17_M_realloc_insertIJS1_EEEvN9__gnu_cxx17__normal_iteratorIPS1_S3_EEDpOT_(%"class.std::vector.6"*, %"struct.std::pair"*, %"struct.std::pair"* dereferenceable(8)) local_unnamed_addr #0 comdat align 2 !dbg !4429 {
  %4 = ptrtoint %"struct.std::pair"* %1 to i64
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4434, metadata !DIExpression()), !dbg !4443
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %2, metadata !4436, metadata !DIExpression()), !dbg !4444
  %5 = tail call i64 @_ZNKSt6vectorISt4pairIiiESaIS1_EE12_M_check_lenEmPKc(%"class.std::vector.6"* %0, i64 1, i8* getelementptr inbounds ([26 x i8], [26 x i8]* @.str.6, i64 0, i64 0)) #9, !dbg !4445
  call void @llvm.dbg.value(metadata i64 %5, metadata !4437, metadata !DIExpression()), !dbg !4446
  %6 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, !dbg !4447
  %7 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, i32 0, i32 0, !dbg !4448
  %8 = load %"struct.std::pair"*, %"struct.std::pair"** %7, align 8, !dbg !4448
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !4438, metadata !DIExpression()), !dbg !4449
  %9 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, i32 0, i32 1, !dbg !4450
  %10 = load %"struct.std::pair"*, %"struct.std::pair"** %9, align 8, !dbg !4450
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %10, metadata !4439, metadata !DIExpression()), !dbg !4451
  %11 = ptrtoint %"struct.std::pair"* %8 to i64, !dbg !4452
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !4435, metadata !DIExpression(DW_OP_deref)), !dbg !4453
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !4454, metadata !DIExpression()), !dbg !4461
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !4460, metadata !DIExpression()), !dbg !4463
  %12 = sub i64 %4, %11, !dbg !4464
  %13 = ashr exact i64 %12, 3, !dbg !4464
  call void @llvm.dbg.value(metadata i64 %13, metadata !4440, metadata !DIExpression()), !dbg !4465
  %14 = tail call %"struct.std::pair"* @_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE11_M_allocateEm(%"struct.std::_Vector_base.7"* %6, i64 %5) #9, !dbg !4466
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4441, metadata !DIExpression()), !dbg !4467
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4442, metadata !DIExpression()), !dbg !4468
  %15 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %14, i64 %13, !dbg !4469
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4397, metadata !DIExpression()), !dbg !4472
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %15, metadata !4405, metadata !DIExpression()), !dbg !4474
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %2, metadata !4406, metadata !DIExpression()), !dbg !4475
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4412, metadata !DIExpression()), !dbg !4476
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %15, metadata !4418, metadata !DIExpression()), !dbg !4478
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %2, metadata !4419, metadata !DIExpression()), !dbg !4479
  %16 = bitcast %"struct.std::pair"* %2 to i64*, !dbg !4480
  %17 = bitcast %"struct.std::pair"* %15 to i64*, !dbg !4480
  %18 = load i64, i64* %16, align 4, !dbg !4480
  store i64 %18, i64* %17, align 4, !dbg !4480
  call void @llvm.dbg.value(metadata %"struct.std::pair"* null, metadata !4442, metadata !DIExpression()), !dbg !4468
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !4435, metadata !DIExpression(DW_OP_deref)), !dbg !4453
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !4481, metadata !DIExpression()), !dbg !4493
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4486, metadata !DIExpression()), !dbg !4495
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4487, metadata !DIExpression()), !dbg !4496
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !4497, metadata !DIExpression()), !dbg !4507
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4502, metadata !DIExpression()), !dbg !4509
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4503, metadata !DIExpression()), !dbg !4510
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !4511, metadata !DIExpression()), !dbg !4520
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4516, metadata !DIExpression()), !dbg !4522
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4517, metadata !DIExpression()), !dbg !4523
  call void @llvm.dbg.value(metadata i8 1, metadata !4518, metadata !DIExpression()), !dbg !4524
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4525, metadata !DIExpression()), !dbg !4532
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %14, metadata !4531, metadata !DIExpression()), !dbg !4534
  br label %19, !dbg !4535

; <label>:19:                                     ; preds = %23, %3
  %20 = phi %"struct.std::pair"* [ %8, %3 ], [ %27, %23 ]
  %21 = phi %"struct.std::pair"* [ %14, %3 ], [ %28, %23 ], !dbg !4538
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %21, metadata !4531, metadata !DIExpression()), !dbg !4534
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4529, metadata !DIExpression(DW_OP_deref)), !dbg !4541
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4530, metadata !DIExpression(DW_OP_deref)), !dbg !4542
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4543, metadata !DIExpression()), !dbg !4550
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4549, metadata !DIExpression()), !dbg !4552
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4553, metadata !DIExpression()), !dbg !4557
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4556, metadata !DIExpression()), !dbg !4559
  %22 = icmp eq %"struct.std::pair"* %20, %1, !dbg !4560
  br i1 %22, label %29, label %23, !dbg !4561

; <label>:23:                                     ; preds = %19
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4529, metadata !DIExpression(DW_OP_deref)), !dbg !4541
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %21, metadata !4562, metadata !DIExpression()), !dbg !4570
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %20, metadata !4567, metadata !DIExpression()), !dbg !4572
  %24 = bitcast %"struct.std::pair"* %20 to i64*, !dbg !4573
  %25 = bitcast %"struct.std::pair"* %21 to i64*, !dbg !4573
  %26 = load i64, i64* %24, align 4, !dbg !4573
  store i64 %26, i64* %25, align 4, !dbg !4573
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4529, metadata !DIExpression(DW_OP_deref)), !dbg !4541
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4574, metadata !DIExpression()), !dbg !4578
  %27 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %20, i64 1, !dbg !4580
  %28 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %21, i64 1, !dbg !4581
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %28, metadata !4531, metadata !DIExpression()), !dbg !4534
  br label %19, !dbg !4582

; <label>:29:                                     ; preds = %19
  %30 = lshr i64 %12, 3, !dbg !4535
  %31 = getelementptr %"struct.std::pair", %"struct.std::pair"* %14, i64 %30, !dbg !4535
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %31, metadata !4531, metadata !DIExpression()), !dbg !4534
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %21, metadata !4531, metadata !DIExpression()), !dbg !4534
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %31, metadata !4442, metadata !DIExpression()), !dbg !4468
  %32 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %31, i64 1, !dbg !4586
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %32, metadata !4442, metadata !DIExpression()), !dbg !4468
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::__normal_iterator"* undef, metadata !4435, metadata !DIExpression(DW_OP_deref)), !dbg !4453
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4481, metadata !DIExpression()), !dbg !4587
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %10, metadata !4486, metadata !DIExpression()), !dbg !4589
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %32, metadata !4487, metadata !DIExpression()), !dbg !4590
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4497, metadata !DIExpression()), !dbg !4591
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %10, metadata !4502, metadata !DIExpression()), !dbg !4593
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %32, metadata !4503, metadata !DIExpression()), !dbg !4594
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %1, metadata !4511, metadata !DIExpression()), !dbg !4595
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %10, metadata !4516, metadata !DIExpression()), !dbg !4597
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %32, metadata !4517, metadata !DIExpression()), !dbg !4598
  call void @llvm.dbg.value(metadata i8 1, metadata !4518, metadata !DIExpression()), !dbg !4599
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %32, metadata !4525, metadata !DIExpression()), !dbg !4600
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %32, metadata !4531, metadata !DIExpression()), !dbg !4602
  br label %33, !dbg !4603

; <label>:33:                                     ; preds = %37, %29
  %34 = phi %"struct.std::pair"* [ %1, %29 ], [ %41, %37 ]
  %35 = phi %"struct.std::pair"* [ %32, %29 ], [ %42, %37 ], !dbg !4604
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %35, metadata !4531, metadata !DIExpression()), !dbg !4602
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4529, metadata !DIExpression(DW_OP_deref)), !dbg !4605
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4530, metadata !DIExpression(DW_OP_deref)), !dbg !4606
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4543, metadata !DIExpression()), !dbg !4607
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4549, metadata !DIExpression()), !dbg !4609
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4553, metadata !DIExpression()), !dbg !4610
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4556, metadata !DIExpression()), !dbg !4612
  %36 = icmp eq %"struct.std::pair"* %34, %10, !dbg !4613
  br i1 %36, label %43, label %37, !dbg !4614

; <label>:37:                                     ; preds = %33
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4529, metadata !DIExpression(DW_OP_deref)), !dbg !4605
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %35, metadata !4562, metadata !DIExpression()), !dbg !4615
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %34, metadata !4567, metadata !DIExpression()), !dbg !4617
  %38 = bitcast %"struct.std::pair"* %34 to i64*, !dbg !4618
  %39 = bitcast %"struct.std::pair"* %35 to i64*, !dbg !4618
  %40 = load i64, i64* %38, align 4, !dbg !4618
  store i64 %40, i64* %39, align 4, !dbg !4618
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4529, metadata !DIExpression(DW_OP_deref)), !dbg !4605
  call void @llvm.dbg.value(metadata %"class.std::move_iterator"* undef, metadata !4574, metadata !DIExpression()), !dbg !4619
  %41 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %34, i64 1, !dbg !4621
  %42 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %35, i64 1, !dbg !4622
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %42, metadata !4531, metadata !DIExpression()), !dbg !4602
  br label %33, !dbg !4623

; <label>:43:                                     ; preds = %33
  %44 = ptrtoint %"struct.std::pair"* %10 to i64, !dbg !4603
  %45 = sub i64 %44, %4, !dbg !4603
  %46 = lshr i64 %45, 3, !dbg !4603
  %47 = getelementptr %"struct.std::pair", %"struct.std::pair"* %32, i64 %46, !dbg !4603
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %47, metadata !4531, metadata !DIExpression()), !dbg !4602
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %35, metadata !4531, metadata !DIExpression()), !dbg !4602
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %47, metadata !4442, metadata !DIExpression()), !dbg !4468
  %48 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, i32 0, i32 2, !dbg !4624
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %6, metadata !3928, metadata !DIExpression()) #8, !dbg !4625
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !3931, metadata !DIExpression()) #8, !dbg !4627
  %49 = icmp eq %"struct.std::pair"* %8, null, !dbg !4628
  br i1 %49, label %52, label %50, !dbg !4629

; <label>:50:                                     ; preds = %43
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %6, metadata !3939, metadata !DIExpression()) #8, !dbg !4630
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !3942, metadata !DIExpression()) #8, !dbg !4632
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %6, metadata !3947, metadata !DIExpression()) #8, !dbg !4633
  call void @llvm.dbg.value(metadata %"struct.std::pair"* %8, metadata !3950, metadata !DIExpression()) #8, !dbg !4635
  %51 = bitcast %"struct.std::pair"* %8 to i8*, !dbg !4636
  call void @llvm.dbg.value(metadata i8* %51, metadata !3957, metadata !DIExpression()) #8, !dbg !4637
  tail call void @free(i8* %51) #7, !dbg !4639
  br label %52, !dbg !4640

; <label>:52:                                     ; preds = %43, %50
  store %"struct.std::pair"* %14, %"struct.std::pair"** %7, align 8, !dbg !4641
  store %"struct.std::pair"* %47, %"struct.std::pair"** %9, align 8, !dbg !4642
  %53 = getelementptr inbounds %"struct.std::pair", %"struct.std::pair"* %14, i64 %5, !dbg !4643
  store %"struct.std::pair"* %53, %"struct.std::pair"** %48, align 8, !dbg !4644
  ret void, !dbg !4645
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr i64 @_ZNKSt6vectorISt4pairIiiESaIS1_EE12_M_check_lenEmPKc(%"class.std::vector.6"*, i64, i8*) local_unnamed_addr #0 comdat align 2 !dbg !4646 {
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4648, metadata !DIExpression()), !dbg !4653
  call void @llvm.dbg.value(metadata i64 %1, metadata !4650, metadata !DIExpression()), !dbg !4654
  call void @llvm.dbg.value(metadata i8* %2, metadata !4651, metadata !DIExpression()), !dbg !4655
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4656, metadata !DIExpression()), !dbg !4659
  %4 = getelementptr inbounds %"class.std::vector.6", %"class.std::vector.6"* %0, i64 0, i32 0, i32 0, i32 1, !dbg !4662
  %5 = bitcast %"struct.std::pair"** %4 to i64*, !dbg !4662
  %6 = load i64, i64* %5, align 8, !dbg !4662
  %7 = bitcast %"class.std::vector.6"* %0 to i64*, !dbg !4663
  %8 = load i64, i64* %7, align 8, !dbg !4663
  %9 = sub i64 %6, %8, !dbg !4664
  %10 = ashr exact i64 %9, 3, !dbg !4664
  %11 = sub nsw i64 2305843009213693951, %10, !dbg !4665
  call void @llvm.dbg.value(metadata i64 %1, metadata !4650, metadata !DIExpression()), !dbg !4654
  %12 = icmp ult i64 %11, %1, !dbg !4666
  br i1 %12, label %13, label %14, !dbg !4667

; <label>:13:                                     ; preds = %3
  call void @llvm.dbg.value(metadata i8* %2, metadata !4668, metadata !DIExpression()) #8, !dbg !4671
  tail call void @mozalloc_abort(i8* %2) #10, !dbg !4673
  unreachable, !dbg !4673

; <label>:14:                                     ; preds = %3
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4656, metadata !DIExpression()), !dbg !4674
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4656, metadata !DIExpression()), !dbg !4676
  %15 = icmp ult i64 %10, %1, !dbg !4678
  %16 = select i1 %15, i64 %1, i64 %10, !dbg !4693
  %17 = add i64 %16, %10, !dbg !4694
  call void @llvm.dbg.value(metadata i64 %17, metadata !4652, metadata !DIExpression()), !dbg !4695
  call void @llvm.dbg.value(metadata %"class.std::vector.6"* %0, metadata !4656, metadata !DIExpression()), !dbg !4696
  %18 = icmp ult i64 %17, %10, !dbg !4698
  %19 = icmp ugt i64 %17, 2305843009213693951, !dbg !4699
  %20 = or i1 %18, %19, !dbg !4700
  %21 = select i1 %20, i64 2305843009213693951, i64 %17, !dbg !4700
  ret i64 %21, !dbg !4701
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"struct.std::pair"* @_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE11_M_allocateEm(%"struct.std::_Vector_base.7"*, i64) local_unnamed_addr #0 comdat align 2 !dbg !4702 {
  call void @llvm.dbg.value(metadata %"struct.std::_Vector_base.7"* %0, metadata !4704, metadata !DIExpression()), !dbg !4706
  call void @llvm.dbg.value(metadata i64 %1, metadata !4705, metadata !DIExpression()), !dbg !4707
  %3 = icmp eq i64 %1, 0, !dbg !4708
  br i1 %3, label %7, label %4, !dbg !4709

; <label>:4:                                      ; preds = %2
  %5 = bitcast %"struct.std::_Vector_base.7"* %0 to %"class.std::allocator.8"*, !dbg !4710
  %6 = tail call %"struct.std::pair"* @_ZNSt16allocator_traitsISaISt4pairIiiEEE8allocateERS2_m(%"class.std::allocator.8"* dereferenceable(1) %5, i64 %1) #9, !dbg !4711
  br label %7, !dbg !4709

; <label>:7:                                      ; preds = %2, %4
  %8 = phi %"struct.std::pair"* [ %6, %4 ], [ null, %2 ], !dbg !4709
  ret %"struct.std::pair"* %8, !dbg !4712
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"struct.std::pair"* @_ZNSt16allocator_traitsISaISt4pairIiiEEE8allocateERS2_m(%"class.std::allocator.8"* dereferenceable(1), i64) local_unnamed_addr #0 comdat align 2 !dbg !4713 {
  call void @llvm.dbg.value(metadata %"class.std::allocator.8"* %0, metadata !4715, metadata !DIExpression()), !dbg !4717
  call void @llvm.dbg.value(metadata i64 %1, metadata !4716, metadata !DIExpression()), !dbg !4718
  %3 = bitcast %"class.std::allocator.8"* %0 to %"class.__gnu_cxx::new_allocator.9"*, !dbg !4719
  %4 = tail call %"struct.std::pair"* @_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE8allocateEmPKv(%"class.__gnu_cxx::new_allocator.9"* nonnull %3, i64 %1, i8* null) #9, !dbg !4720
  ret %"struct.std::pair"* %4, !dbg !4721
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr %"struct.std::pair"* @_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE8allocateEmPKv(%"class.__gnu_cxx::new_allocator.9"*, i64, i8*) local_unnamed_addr #0 comdat align 2 !dbg !4722 {
  call void @llvm.dbg.value(metadata %"class.__gnu_cxx::new_allocator.9"* %0, metadata !4724, metadata !DIExpression()), !dbg !4727
  call void @llvm.dbg.value(metadata i64 %1, metadata !4725, metadata !DIExpression()), !dbg !4728
  call void @llvm.dbg.value(metadata i8* %2, metadata !4726, metadata !DIExpression()), !dbg !4729
  %4 = icmp ugt i64 %1, 2305843009213693951, !dbg !4730
  br i1 %4, label %5, label %6, !dbg !4732

; <label>:5:                                      ; preds = %3
  tail call void @mozalloc_abort(i8* getelementptr inbounds ([27 x i8], [27 x i8]* @.str.5, i64 0, i64 0)) #10, !dbg !4733
  unreachable, !dbg !4733

; <label>:6:                                      ; preds = %3
  %7 = shl i64 %1, 3, !dbg !4735
  call void @llvm.dbg.value(metadata i64 %7, metadata !4328, metadata !DIExpression()) #8, !dbg !4736
  %8 = tail call noalias i8* @moz_xmalloc(i64 %7) #7, !dbg !4738
  %9 = bitcast i8* %8 to %"struct.std::pair"*, !dbg !4739
  ret %"struct.std::pair"* %9, !dbg !4740
}

; Function Attrs: minsize nounwind optsize sspstrong uwtable
define linkonce_odr hidden void @_ZN7mozilla21ScopedCloseFileTraits7releaseEP8_IO_FILE(%struct._IO_FILE*) local_unnamed_addr #0 comdat align 2 !dbg !4741 {
  call void @llvm.dbg.value(metadata %struct._IO_FILE* %0, metadata !4743, metadata !DIExpression()), !dbg !4744
  %2 = icmp eq %struct._IO_FILE* %0, null, !dbg !4745
  br i1 %2, label %5, label %3, !dbg !4747

; <label>:3:                                      ; preds = %1
  %4 = tail call i32 @fclose(%struct._IO_FILE* nonnull %0) #9, !dbg !4748
  br label %5, !dbg !4750

; <label>:5:                                      ; preds = %1, %3
  ret void, !dbg !4751
}

; Function Attrs: minsize nounwind optsize
declare i32 @fclose(%struct._IO_FILE* nocapture) local_unnamed_addr #2

; Function Attrs: nounwind readnone speculatable
declare void @llvm.dbg.value(metadata, metadata, metadata) #1

; Function Attrs: argmemonly nounwind
declare void @llvm.memset.p0i8.i64(i8* nocapture writeonly, i8, i64, i1) #3

attributes #0 = { minsize nounwind optsize sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { minsize nounwind optsize "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { argmemonly nounwind }
attributes #4 = { minsize optsize "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { inlinehint minsize nounwind optsize sspstrong uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #6 = { minsize noreturn optsize "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #7 = { minsize nounwind optsize }
attributes #8 = { nounwind }
attributes #9 = { minsize optsize }
attributes #10 = { minsize noreturn nounwind optsize }

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2903, !2904, !2905, !2906}
!llvm.ident = !{!2907}

!0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus, file: !1, producer: "clang version 7.0.0 (tags/RELEASE_700/final)", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, enums: !2, retainedTypes: !36, globals: !1567, imports: !1572)
!1 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/widget/LSBUtils.cpp", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2 = !{!3, !11, !17, !24, !29}
!3 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "_Lock_policy", scope: !5, file: !4, line: 49, baseType: !6, size: 32, elements: !7, identifier: "_ZTSN9__gnu_cxx12_Lock_policyE")
!4 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/ext/concurrence.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!5 = !DINamespace(name: "__gnu_cxx", scope: null)
!6 = !DIBasicType(name: "unsigned int", size: 32, encoding: DW_ATE_unsigned)
!7 = !{!8, !9, !10}
!8 = !DIEnumerator(name: "_S_single", value: 0, isUnsigned: true)
!9 = !DIEnumerator(name: "_S_mutex", value: 1, isUnsigned: true)
!10 = !DIEnumerator(name: "_S_atomic", value: 2, isUnsigned: true)
!11 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "_Rb_tree_color", scope: !13, file: !12, line: 99, baseType: !6, size: 32, elements: !14, identifier: "_ZTSSt14_Rb_tree_color")
!12 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_tree.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!13 = !DINamespace(name: "std", scope: null)
!14 = !{!15, !16}
!15 = !DIEnumerator(name: "_S_red", value: 0, isUnsigned: true)
!16 = !DIEnumerator(name: "_S_black", value: 1, isUnsigned: true)
!17 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "StorageType", scope: !19, file: !18, line: 20, baseType: !6, size: 32, elements: !21, identifier: "_ZTSN7mozilla6detail11StorageTypeE")
!18 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/Pair.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!19 = !DINamespace(name: "detail", scope: !20)
!20 = !DINamespace(name: "mozilla", scope: null)
!21 = !{!22, !23}
!22 = !DIEnumerator(name: "AsBase", value: 0, isUnsigned: true)
!23 = !DIEnumerator(name: "AsMember", value: 1, isUnsigned: true)
!24 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "Voidness", scope: !19, file: !25, line: 882, baseType: !6, size: 32, elements: !26, identifier: "_ZTSN7mozilla6detail8VoidnessE")
!25 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/TypeTraits.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!26 = !{!27, !28}
!27 = !DIEnumerator(name: "TIsVoid", value: 0, isUnsigned: true)
!28 = !DIEnumerator(name: "TIsNotVoid", value: 1, isUnsigned: true)
!29 = !DICompositeType(tag: DW_TAG_enumeration_type, scope: !31, file: !30, line: 158, baseType: !6, size: 32, elements: !34, identifier: "_ZTSNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEUt_E")
!30 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/basic_string.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!31 = !DICompositeType(tag: DW_TAG_class_type, name: "basic_string<char, std::char_traits<char>, std::allocator<char> >", scope: !33, file: !32, line: 1607, flags: DIFlagFwdDecl, identifier: "_ZTSNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEE")
!32 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/basic_string.tcc", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!33 = !DINamespace(name: "__cxx11", scope: !13, exportSymbols: true)
!34 = !{!35}
!35 = !DIEnumerator(name: "_S_local_capacity", value: 15, isUnsigned: true)
!36 = !{!37, !47, !302, !321, !506, !988, !1159, !1208, !178, !1008, !175, !1260, !1261, !1262, !70, !1518, !31, !951}
!37 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !39, file: !38, line: 319, baseType: !40)
!38 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/nsTSubstring.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!39 = !DICompositeType(tag: DW_TAG_class_type, name: "nsTSubstring<char>", file: !38, line: 1350, flags: DIFlagFwdDecl, identifier: "_ZTS12nsTSubstringIcE")
!40 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !42, file: !41, line: 120, baseType: !43)
!41 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/nsTStringRepr.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!42 = !DICompositeType(tag: DW_TAG_class_type, name: "nsTStringRepr<char>", scope: !19, file: !41, line: 304, flags: DIFlagFwdDecl, identifier: "_ZTSN7mozilla6detail13nsTStringReprIcEE")
!43 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint32_t", file: !44, line: 26, baseType: !45)
!44 = !DIFile(filename: "/usr/include/bits/stdint-uintn.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!45 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint32_t", file: !46, line: 41, baseType: !6)
!46 = !DIFile(filename: "/usr/include/bits/types.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!47 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !48, size: 64)
!48 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tp_alloc_type", scope: !50, file: !49, line: 84, baseType: !299)
!49 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_vector.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!50 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Vector_base<std::pair<int, int>, std::allocator<std::pair<int, int> > >", scope: !13, file: !49, line: 81, size: 192, flags: DIFlagTypePassByReference, elements: !51, templateParams: !298, identifier: "_ZTSSt12_Vector_baseISt4pairIiiESaIS1_EE")
!51 = !{!52, !252, !257, !262, !266, !269, !274, !277, !280, !283, !287, !290, !291, !294, !297}
!52 = !DIDerivedType(tag: DW_TAG_member, name: "_M_impl", scope: !50, file: !49, line: 290, baseType: !53, size: 192)
!53 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Vector_impl", scope: !50, file: !49, line: 88, size: 192, flags: DIFlagTypePassByReference, elements: !54, identifier: "_ZTSNSt12_Vector_baseISt4pairIiiESaIS1_EE12_Vector_implE")
!54 = !{!55, !56, !233, !234, !235, !239, !244, !248}
!55 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !53, baseType: !48, extraData: i32 0)
!56 = !DIDerivedType(tag: DW_TAG_member, name: "_M_start", scope: !53, file: !49, line: 91, baseType: !57, size: 64)
!57 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !50, file: !49, line: 86, baseType: !58)
!58 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !60, file: !59, line: 59, baseType: !69)
!59 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/ext/alloc_traits.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!60 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__alloc_traits<std::allocator<std::pair<int, int> >, std::pair<int, int> >", scope: !5, file: !59, line: 50, size: 8, flags: DIFlagTypePassByValue, elements: !61, templateParams: !231, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_EE")
!61 = !{!62, !217, !220, !224, !227, !228, !229, !230}
!62 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !60, baseType: !63, extraData: i32 0)
!63 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "allocator_traits<std::allocator<std::pair<int, int> > >", scope: !13, file: !64, line: 384, size: 8, flags: DIFlagTypePassByValue, elements: !65, templateParams: !215, identifier: "_ZTSSt16allocator_traitsISaISt4pairIiiEEE")
!64 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/alloc_traits.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!65 = !{!66, !199, !203, !206, !212}
!66 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE8allocateERS2_m", scope: !63, file: !64, line: 435, type: !67, isLocal: false, isDefinition: false, scopeLine: 435, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!67 = !DISubroutineType(types: !68)
!68 = !{!69, !138, !198}
!69 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !63, file: !64, line: 392, baseType: !70)
!70 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !71, size: 64)
!71 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "pair<int, int>", scope: !13, file: !72, line: 208, size: 64, flags: DIFlagTypePassByValue, elements: !73, templateParams: !135, identifier: "_ZTSSt4pairIiiE")
!72 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_pair.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!73 = !{!74, !95, !96, !97, !103, !107, !123, !132}
!74 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !71, baseType: !75, flags: DIFlagPrivate, extraData: i32 0)
!75 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "__pair_base<int, int>", scope: !13, file: !72, line: 190, size: 8, flags: DIFlagTypePassByValue, elements: !76, templateParams: !91, identifier: "_ZTSSt11__pair_baseIiiE")
!76 = !{!77, !81, !82, !87}
!77 = !DISubprogram(name: "__pair_base", scope: !75, file: !72, line: 194, type: !78, isLocal: false, isDefinition: false, scopeLine: 194, flags: DIFlagPrototyped, isOptimized: true)
!78 = !DISubroutineType(types: !79)
!79 = !{null, !80}
!80 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !75, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!81 = !DISubprogram(name: "~__pair_base", scope: !75, file: !72, line: 195, type: !78, isLocal: false, isDefinition: false, scopeLine: 195, flags: DIFlagPrototyped, isOptimized: true)
!82 = !DISubprogram(name: "__pair_base", scope: !75, file: !72, line: 196, type: !83, isLocal: false, isDefinition: false, scopeLine: 196, flags: DIFlagPrototyped, isOptimized: true)
!83 = !DISubroutineType(types: !84)
!84 = !{null, !80, !85}
!85 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !86, size: 64)
!86 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !75)
!87 = !DISubprogram(name: "operator=", linkageName: "_ZNSt11__pair_baseIiiEaSERKS0_", scope: !75, file: !72, line: 197, type: !88, isLocal: false, isDefinition: false, scopeLine: 197, flags: DIFlagPrototyped, isOptimized: true)
!88 = !DISubroutineType(types: !89)
!89 = !{!90, !80, !85}
!90 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !75, size: 64)
!91 = !{!92, !94}
!92 = !DITemplateTypeParameter(name: "_U1", type: !93)
!93 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!94 = !DITemplateTypeParameter(name: "_U2", type: !93)
!95 = !DIDerivedType(tag: DW_TAG_member, name: "first", scope: !71, file: !72, line: 214, baseType: !93, size: 32)
!96 = !DIDerivedType(tag: DW_TAG_member, name: "second", scope: !71, file: !72, line: 215, baseType: !93, size: 32, offset: 32)
!97 = !DISubprogram(name: "pair", scope: !71, file: !72, line: 303, type: !98, isLocal: false, isDefinition: false, scopeLine: 303, flags: DIFlagPrototyped, isOptimized: true)
!98 = !DISubroutineType(types: !99)
!99 = !{null, !100, !101}
!100 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !71, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!101 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !102, size: 64)
!102 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !71)
!103 = !DISubprogram(name: "pair", scope: !71, file: !72, line: 304, type: !104, isLocal: false, isDefinition: false, scopeLine: 304, flags: DIFlagPrototyped, isOptimized: true)
!104 = !DISubroutineType(types: !105)
!105 = !{null, !100, !106}
!106 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !71, size: 64)
!107 = !DISubprogram(name: "operator=", linkageName: "_ZNSt4pairIiiEaSERKS0_", scope: !71, file: !72, line: 378, type: !108, isLocal: false, isDefinition: false, scopeLine: 378, flags: DIFlagPrototyped, isOptimized: true)
!108 = !DISubroutineType(types: !109)
!109 = !{!110, !100, !111}
!110 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !71, size: 64)
!111 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !113, file: !112, line: 1970, baseType: !101)
!112 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/type_traits", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!113 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<true, const std::pair<int, int> &, const std::__nonesuch_no_braces &>", scope: !13, file: !112, line: 1969, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !115, identifier: "_ZTSSt11conditionalILb1ERKSt4pairIiiERKSt20__nonesuch_no_bracesE")
!114 = !{}
!115 = !{!116, !118, !119}
!116 = !DITemplateValueParameter(name: "_Cond", type: !117, value: i8 1)
!117 = !DIBasicType(name: "bool", size: 8, encoding: DW_ATE_boolean)
!118 = !DITemplateTypeParameter(name: "_Iftrue", type: !101)
!119 = !DITemplateTypeParameter(name: "_Iffalse", type: !120)
!120 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !121, size: 64)
!121 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !122)
!122 = !DICompositeType(tag: DW_TAG_structure_type, name: "__nonesuch_no_braces", scope: !13, file: !72, line: 185, flags: DIFlagFwdDecl, identifier: "_ZTSSt20__nonesuch_no_braces")
!123 = !DISubprogram(name: "operator=", linkageName: "_ZNSt4pairIiiEaSEOS0_", scope: !71, file: !72, line: 389, type: !124, isLocal: false, isDefinition: false, scopeLine: 389, flags: DIFlagPrototyped, isOptimized: true)
!124 = !DISubroutineType(types: !125)
!125 = !{!110, !100, !126}
!126 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !127, file: !112, line: 1970, baseType: !106)
!127 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<true, std::pair<int, int> &&, std::__nonesuch_no_braces &&>", scope: !13, file: !112, line: 1969, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !128, identifier: "_ZTSSt11conditionalILb1EOSt4pairIiiEOSt20__nonesuch_no_bracesE")
!128 = !{!116, !129, !130}
!129 = !DITemplateTypeParameter(name: "_Iftrue", type: !106)
!130 = !DITemplateTypeParameter(name: "_Iffalse", type: !131)
!131 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !122, size: 64)
!132 = !DISubprogram(name: "swap", linkageName: "_ZNSt4pairIiiE4swapERS0_", scope: !71, file: !72, line: 424, type: !133, isLocal: false, isDefinition: false, scopeLine: 424, flags: DIFlagPrototyped, isOptimized: true)
!133 = !DISubroutineType(types: !134)
!134 = !{null, !100, !110}
!135 = !{!136, !137}
!136 = !DITemplateTypeParameter(name: "_T1", type: !93)
!137 = !DITemplateTypeParameter(name: "_T2", type: !93)
!138 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !139, size: 64)
!139 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !63, file: !64, line: 387, baseType: !140)
!140 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "allocator<std::pair<int, int> >", scope: !13, file: !141, line: 108, size: 8, flags: DIFlagTypePassByReference, elements: !142, templateParams: !186, identifier: "_ZTSSaISt4pairIiiEE")
!141 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/allocator.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!142 = !{!143, !188, !192, !197}
!143 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !140, baseType: !144, flags: DIFlagPublic, extraData: i32 0)
!144 = !DIDerivedType(tag: DW_TAG_typedef, name: "__allocator_base<std::pair<int, int> >", scope: !13, file: !145, line: 48, baseType: !146)
!145 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/x86_64-pc-linux-gnu/bits/c++allocator.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!146 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "new_allocator<std::pair<int, int> >", scope: !5, file: !147, line: 58, size: 8, flags: DIFlagTypePassByReference, elements: !148, templateParams: !186, identifier: "_ZTSN9__gnu_cxx13new_allocatorISt4pairIiiEEE")
!147 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/ext/new_allocator.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!148 = !{!149, !153, !158, !159, !165, !171, !180, !183}
!149 = !DISubprogram(name: "new_allocator", scope: !146, file: !147, line: 79, type: !150, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!150 = !DISubroutineType(types: !151)
!151 = !{null, !152}
!152 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !146, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!153 = !DISubprogram(name: "new_allocator", scope: !146, file: !147, line: 81, type: !154, isLocal: false, isDefinition: false, scopeLine: 81, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!154 = !DISubroutineType(types: !155)
!155 = !{null, !152, !156}
!156 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !157, size: 64)
!157 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !146)
!158 = !DISubprogram(name: "~new_allocator", scope: !146, file: !147, line: 86, type: !150, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!159 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt4pairIiiEE7addressERS2_", scope: !146, file: !147, line: 89, type: !160, isLocal: false, isDefinition: false, scopeLine: 89, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!160 = !DISubroutineType(types: !161)
!161 = !{!162, !163, !164}
!162 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !146, file: !147, line: 63, baseType: !70)
!163 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !157, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!164 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !146, file: !147, line: 65, baseType: !110)
!165 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt4pairIiiEE7addressERKS2_", scope: !146, file: !147, line: 93, type: !166, isLocal: false, isDefinition: false, scopeLine: 93, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!166 = !DISubroutineType(types: !167)
!167 = !{!168, !163, !170}
!168 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_pointer", scope: !146, file: !147, line: 64, baseType: !169)
!169 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !102, size: 64)
!170 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !146, file: !147, line: 66, baseType: !101)
!171 = !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE8allocateEmPKv", scope: !146, file: !147, line: 99, type: !172, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!172 = !DISubroutineType(types: !173)
!173 = !{!162, !152, !174, !178}
!174 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !147, line: 61, baseType: !175)
!175 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", scope: !13, file: !176, line: 238, baseType: !177)
!176 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/x86_64-pc-linux-gnu/bits/c++config.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!177 = !DIBasicType(name: "long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!178 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !179, size: 64)
!179 = !DIDerivedType(tag: DW_TAG_const_type, baseType: null)
!180 = !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE10deallocateEPS2_m", scope: !146, file: !147, line: 116, type: !181, isLocal: false, isDefinition: false, scopeLine: 116, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!181 = !DISubroutineType(types: !182)
!182 = !{null, !152, !162, !174}
!183 = !DISubprogram(name: "max_size", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt4pairIiiEE8max_sizeEv", scope: !146, file: !147, line: 129, type: !184, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!184 = !DISubroutineType(types: !185)
!185 = !{!174, !163}
!186 = !{!187}
!187 = !DITemplateTypeParameter(name: "_Tp", type: !71)
!188 = !DISubprogram(name: "allocator", scope: !140, file: !141, line: 131, type: !189, isLocal: false, isDefinition: false, scopeLine: 131, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!189 = !DISubroutineType(types: !190)
!190 = !{null, !191}
!191 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !140, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!192 = !DISubprogram(name: "allocator", scope: !140, file: !141, line: 133, type: !193, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!193 = !DISubroutineType(types: !194)
!194 = !{null, !191, !195}
!195 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !196, size: 64)
!196 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !140)
!197 = !DISubprogram(name: "~allocator", scope: !140, file: !141, line: 139, type: !189, isLocal: false, isDefinition: false, scopeLine: 139, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!198 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !64, line: 407, baseType: !175)
!199 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE8allocateERS2_mPKv", scope: !63, file: !64, line: 449, type: !200, isLocal: false, isDefinition: false, scopeLine: 449, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!200 = !DISubroutineType(types: !201)
!201 = !{!69, !138, !198, !202}
!202 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_void_pointer", file: !64, line: 401, baseType: !178)
!203 = !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE10deallocateERS2_PS1_m", scope: !63, file: !64, line: 461, type: !204, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!204 = !DISubroutineType(types: !205)
!205 = !{null, !138, !69, !198}
!206 = !DISubprogram(name: "max_size", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE8max_sizeERKS2_", scope: !63, file: !64, line: 495, type: !207, isLocal: false, isDefinition: false, scopeLine: 495, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!207 = !DISubroutineType(types: !208)
!208 = !{!209, !210}
!209 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !63, file: !64, line: 407, baseType: !175)
!210 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !211, size: 64)
!211 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !139)
!212 = !DISubprogram(name: "select_on_container_copy_construction", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE37select_on_container_copy_constructionERKS2_", scope: !63, file: !64, line: 504, type: !213, isLocal: false, isDefinition: false, scopeLine: 504, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!213 = !DISubroutineType(types: !214)
!214 = !{!139, !210}
!215 = !{!216}
!216 = !DITemplateTypeParameter(name: "_Alloc", type: !140)
!217 = !DISubprogram(name: "_S_select_on_copy", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E17_S_select_on_copyERKS3_", scope: !60, file: !59, line: 94, type: !218, isLocal: false, isDefinition: false, scopeLine: 94, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!218 = !DISubroutineType(types: !219)
!219 = !{!140, !195}
!220 = !DISubprogram(name: "_S_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E10_S_on_swapERS3_S5_", scope: !60, file: !59, line: 97, type: !221, isLocal: false, isDefinition: false, scopeLine: 97, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!221 = !DISubroutineType(types: !222)
!222 = !{null, !223, !223}
!223 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !140, size: 64)
!224 = !DISubprogram(name: "_S_propagate_on_copy_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E27_S_propagate_on_copy_assignEv", scope: !60, file: !59, line: 100, type: !225, isLocal: false, isDefinition: false, scopeLine: 100, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!225 = !DISubroutineType(types: !226)
!226 = !{!117}
!227 = !DISubprogram(name: "_S_propagate_on_move_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E27_S_propagate_on_move_assignEv", scope: !60, file: !59, line: 103, type: !225, isLocal: false, isDefinition: false, scopeLine: 103, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!228 = !DISubprogram(name: "_S_propagate_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E20_S_propagate_on_swapEv", scope: !60, file: !59, line: 106, type: !225, isLocal: false, isDefinition: false, scopeLine: 106, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!229 = !DISubprogram(name: "_S_always_equal", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E15_S_always_equalEv", scope: !60, file: !59, line: 109, type: !225, isLocal: false, isDefinition: false, scopeLine: 109, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!230 = !DISubprogram(name: "_S_nothrow_move", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E15_S_nothrow_moveEv", scope: !60, file: !59, line: 112, type: !225, isLocal: false, isDefinition: false, scopeLine: 112, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!231 = !{!216, !232}
!232 = !DITemplateTypeParameter(type: !71)
!233 = !DIDerivedType(tag: DW_TAG_member, name: "_M_finish", scope: !53, file: !49, line: 92, baseType: !57, size: 64, offset: 64)
!234 = !DIDerivedType(tag: DW_TAG_member, name: "_M_end_of_storage", scope: !53, file: !49, line: 93, baseType: !57, size: 64, offset: 128)
!235 = !DISubprogram(name: "_Vector_impl", scope: !53, file: !49, line: 95, type: !236, isLocal: false, isDefinition: false, scopeLine: 95, flags: DIFlagPrototyped, isOptimized: true)
!236 = !DISubroutineType(types: !237)
!237 = !{null, !238}
!238 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !53, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!239 = !DISubprogram(name: "_Vector_impl", scope: !53, file: !49, line: 99, type: !240, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPrototyped, isOptimized: true)
!240 = !DISubroutineType(types: !241)
!241 = !{null, !238, !242}
!242 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !243, size: 64)
!243 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !48)
!244 = !DISubprogram(name: "_Vector_impl", scope: !53, file: !49, line: 104, type: !245, isLocal: false, isDefinition: false, scopeLine: 104, flags: DIFlagPrototyped, isOptimized: true)
!245 = !DISubroutineType(types: !246)
!246 = !{null, !238, !247}
!247 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !48, size: 64)
!248 = !DISubprogram(name: "_M_swap_data", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE12_Vector_impl12_M_swap_dataERS4_", scope: !53, file: !49, line: 110, type: !249, isLocal: false, isDefinition: false, scopeLine: 110, flags: DIFlagPrototyped, isOptimized: true)
!249 = !DISubroutineType(types: !250)
!250 = !{null, !238, !251}
!251 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !53, size: 64)
!252 = !DISubprogram(name: "_M_get_Tp_allocator", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE19_M_get_Tp_allocatorEv", scope: !50, file: !49, line: 237, type: !253, isLocal: false, isDefinition: false, scopeLine: 237, flags: DIFlagPrototyped, isOptimized: true)
!253 = !DISubroutineType(types: !254)
!254 = !{!255, !256}
!255 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !48, size: 64)
!256 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !50, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!257 = !DISubprogram(name: "_M_get_Tp_allocator", linkageName: "_ZNKSt12_Vector_baseISt4pairIiiESaIS1_EE19_M_get_Tp_allocatorEv", scope: !50, file: !49, line: 241, type: !258, isLocal: false, isDefinition: false, scopeLine: 241, flags: DIFlagPrototyped, isOptimized: true)
!258 = !DISubroutineType(types: !259)
!259 = !{!242, !260}
!260 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !261, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!261 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !50)
!262 = !DISubprogram(name: "get_allocator", linkageName: "_ZNKSt12_Vector_baseISt4pairIiiESaIS1_EE13get_allocatorEv", scope: !50, file: !49, line: 245, type: !263, isLocal: false, isDefinition: false, scopeLine: 245, flags: DIFlagPrototyped, isOptimized: true)
!263 = !DISubroutineType(types: !264)
!264 = !{!265, !260}
!265 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !50, file: !49, line: 234, baseType: !140)
!266 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 248, type: !267, isLocal: false, isDefinition: false, scopeLine: 248, flags: DIFlagPrototyped, isOptimized: true)
!267 = !DISubroutineType(types: !268)
!268 = !{null, !256}
!269 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 251, type: !270, isLocal: false, isDefinition: false, scopeLine: 251, flags: DIFlagPrototyped, isOptimized: true)
!270 = !DISubroutineType(types: !271)
!271 = !{null, !256, !272}
!272 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !273, size: 64)
!273 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !265)
!274 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 254, type: !275, isLocal: false, isDefinition: false, scopeLine: 254, flags: DIFlagPrototyped, isOptimized: true)
!275 = !DISubroutineType(types: !276)
!276 = !{null, !256, !175}
!277 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 258, type: !278, isLocal: false, isDefinition: false, scopeLine: 258, flags: DIFlagPrototyped, isOptimized: true)
!278 = !DISubroutineType(types: !279)
!279 = !{null, !256, !175, !272}
!280 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 263, type: !281, isLocal: false, isDefinition: false, scopeLine: 263, flags: DIFlagPrototyped, isOptimized: true)
!281 = !DISubroutineType(types: !282)
!282 = !{null, !256, !247}
!283 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 266, type: !284, isLocal: false, isDefinition: false, scopeLine: 266, flags: DIFlagPrototyped, isOptimized: true)
!284 = !DISubroutineType(types: !285)
!285 = !{null, !256, !286}
!286 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !50, size: 64)
!287 = !DISubprogram(name: "_Vector_base", scope: !50, file: !49, line: 270, type: !288, isLocal: false, isDefinition: false, scopeLine: 270, flags: DIFlagPrototyped, isOptimized: true)
!288 = !DISubroutineType(types: !289)
!289 = !{null, !256, !286, !272}
!290 = !DISubprogram(name: "~_Vector_base", scope: !50, file: !49, line: 283, type: !267, isLocal: false, isDefinition: false, scopeLine: 283, flags: DIFlagPrototyped, isOptimized: true)
!291 = !DISubprogram(name: "_M_allocate", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE11_M_allocateEm", scope: !50, file: !49, line: 293, type: !292, isLocal: false, isDefinition: false, scopeLine: 293, flags: DIFlagPrototyped, isOptimized: true)
!292 = !DISubroutineType(types: !293)
!293 = !{!57, !256, !175}
!294 = !DISubprogram(name: "_M_deallocate", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE13_M_deallocateEPS1_m", scope: !50, file: !49, line: 300, type: !295, isLocal: false, isDefinition: false, scopeLine: 300, flags: DIFlagPrototyped, isOptimized: true)
!295 = !DISubroutineType(types: !296)
!296 = !{null, !256, !57, !175}
!297 = !DISubprogram(name: "_M_create_storage", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE17_M_create_storageEm", scope: !50, file: !49, line: 309, type: !275, isLocal: false, isDefinition: false, scopeLine: 309, flags: DIFlagPrivate | DIFlagPrototyped, isOptimized: true)
!298 = !{!187, !216}
!299 = !DIDerivedType(tag: DW_TAG_typedef, name: "other", scope: !300, file: !59, line: 117, baseType: !301)
!300 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "rebind<std::pair<int, int> >", scope: !60, file: !59, line: 116, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !186, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaISt4pairIiiEES2_E6rebindIS2_EE")
!301 = !DIDerivedType(tag: DW_TAG_typedef, name: "rebind_alloc<std::pair<int, int> >", scope: !63, file: !64, line: 422, baseType: !140)
!302 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Link_type", scope: !303, file: !12, line: 465, baseType: !550)
!303 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "_Rb_tree<std::__cxx11::basic_string<char>, std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> >, std::_Select1st<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, std::less<std::__cxx11::basic_string<char> >, std::allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !12, line: 444, size: 384, flags: DIFlagTypePassByReference, elements: !304, templateParams: !983, identifier: "_ZTSSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE")
!304 = !{!305, !659, !664, !671, !675, !678, !681, !682, !683, !688, !692, !693, !694, !695, !696, !697, !701, !704, !705, !712, !715, !718, !721, !722, !723, !726, !729, !733, !737, !738, !739, !800, !801, !806, !807, !812, !815, !818, !822, !823, !826, !829, !830, !831, !834, !839, !842, !845, !848, !852, !855, !858, !859, !863, !866, !869, !872, !873, !874, !880, !885, !886, !887, !890, !894, !895, !898, !901, !904, !907, !910, !914, !917, !921, !922, !925, !928, !931, !932, !933, !934, !935, !939, !943, !944, !947, !965, !981, !982}
!305 = !DIDerivedType(tag: DW_TAG_member, name: "_M_impl", scope: !303, file: !12, line: 724, baseType: !306, size: 384, flags: DIFlagProtected)
!306 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_impl<std::less<std::__cxx11::basic_string<char> >, true>", scope: !303, file: !12, line: 692, size: 384, flags: DIFlagTypePassByReference, elements: !307, templateParams: !657, identifier: "_ZTSNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE13_Rb_tree_implISC_Lb1EEE")
!307 = !{!308, !582, !622, !640, !644, !649, !653}
!308 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !306, baseType: !309, extraData: i32 0)
!309 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Node_allocator", scope: !303, file: !12, line: 447, baseType: !310)
!310 = !DIDerivedType(tag: DW_TAG_typedef, name: "other", scope: !311, file: !59, line: 117, baseType: !529)
!311 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "rebind<std::_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !312, file: !59, line: 116, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !461, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E6rebindISt13_Rb_tree_nodeIS9_EEE")
!312 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__alloc_traits<std::allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !5, file: !59, line: 50, size: 8, flags: DIFlagTypePassByValue, elements: !313, templateParams: !459, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_EE")
!313 = !{!314, !447, !450, !454, !455, !456, !457, !458}
!314 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !312, baseType: !315, extraData: i32 0)
!315 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "allocator_traits<std::allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !64, line: 384, size: 8, flags: DIFlagTypePassByValue, elements: !316, templateParams: !445, identifier: "_ZTSSt16allocator_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE")
!316 = !{!317, !430, !433, !436, !442}
!317 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE8allocateERS9_m", scope: !315, file: !64, line: 435, type: !318, isLocal: false, isDefinition: false, scopeLine: 435, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!318 = !DISubroutineType(types: !319)
!319 = !{!320, !379, !198}
!320 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !315, file: !64, line: 392, baseType: !321)
!321 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !322, size: 64)
!322 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> >", scope: !13, file: !72, line: 208, size: 512, flags: DIFlagTypePassByReference, elements: !323, templateParams: !376, identifier: "_ZTSSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_E")
!323 = !{!324, !345, !346, !347, !353, !357, !366, !373}
!324 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !322, baseType: !325, flags: DIFlagPrivate, extraData: i32 0)
!325 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "__pair_base<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> >", scope: !13, file: !72, line: 190, size: 8, flags: DIFlagTypePassByValue, elements: !326, templateParams: !341, identifier: "_ZTSSt11__pair_baseIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_E")
!326 = !{!327, !331, !332, !337}
!327 = !DISubprogram(name: "__pair_base", scope: !325, file: !72, line: 194, type: !328, isLocal: false, isDefinition: false, scopeLine: 194, flags: DIFlagPrototyped, isOptimized: true)
!328 = !DISubroutineType(types: !329)
!329 = !{null, !330}
!330 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !325, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!331 = !DISubprogram(name: "~__pair_base", scope: !325, file: !72, line: 195, type: !328, isLocal: false, isDefinition: false, scopeLine: 195, flags: DIFlagPrototyped, isOptimized: true)
!332 = !DISubprogram(name: "__pair_base", scope: !325, file: !72, line: 196, type: !333, isLocal: false, isDefinition: false, scopeLine: 196, flags: DIFlagPrototyped, isOptimized: true)
!333 = !DISubroutineType(types: !334)
!334 = !{null, !330, !335}
!335 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !336, size: 64)
!336 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !325)
!337 = !DISubprogram(name: "operator=", linkageName: "_ZNSt11__pair_baseIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_EaSERKS7_", scope: !325, file: !72, line: 197, type: !338, isLocal: false, isDefinition: false, scopeLine: 197, flags: DIFlagPrototyped, isOptimized: true)
!338 = !DISubroutineType(types: !339)
!339 = !{!340, !330, !335}
!340 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !325, size: 64)
!341 = !{!342, !344}
!342 = !DITemplateTypeParameter(name: "_U1", type: !343)
!343 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !31)
!344 = !DITemplateTypeParameter(name: "_U2", type: !31)
!345 = !DIDerivedType(tag: DW_TAG_member, name: "first", scope: !322, file: !72, line: 214, baseType: !343, size: 256)
!346 = !DIDerivedType(tag: DW_TAG_member, name: "second", scope: !322, file: !72, line: 215, baseType: !31, size: 256, offset: 256)
!347 = !DISubprogram(name: "pair", scope: !322, file: !72, line: 303, type: !348, isLocal: false, isDefinition: false, scopeLine: 303, flags: DIFlagPrototyped, isOptimized: true)
!348 = !DISubroutineType(types: !349)
!349 = !{null, !350, !351}
!350 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !322, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!351 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !352, size: 64)
!352 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !322)
!353 = !DISubprogram(name: "pair", scope: !322, file: !72, line: 304, type: !354, isLocal: false, isDefinition: false, scopeLine: 304, flags: DIFlagPrototyped, isOptimized: true)
!354 = !DISubroutineType(types: !355)
!355 = !{null, !350, !356}
!356 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !322, size: 64)
!357 = !DISubprogram(name: "operator=", linkageName: "_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_EaSERKSt20__nonesuch_no_braces", scope: !322, file: !72, line: 378, type: !358, isLocal: false, isDefinition: false, scopeLine: 378, flags: DIFlagPrototyped, isOptimized: true)
!358 = !DISubroutineType(types: !359)
!359 = !{!360, !350, !361}
!360 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !322, size: 64)
!361 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !362, file: !112, line: 1975, baseType: !120)
!362 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<false, const std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > &, const std::__nonesuch_no_braces &>", scope: !13, file: !112, line: 1974, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !363, identifier: "_ZTSSt11conditionalILb0ERKSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_ERKSt20__nonesuch_no_bracesE")
!363 = !{!364, !365, !119}
!364 = !DITemplateValueParameter(name: "_Cond", type: !117, value: i8 0)
!365 = !DITemplateTypeParameter(name: "_Iftrue", type: !351)
!366 = !DISubprogram(name: "operator=", linkageName: "_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_EaSEOSt20__nonesuch_no_braces", scope: !322, file: !72, line: 389, type: !367, isLocal: false, isDefinition: false, scopeLine: 389, flags: DIFlagPrototyped, isOptimized: true)
!367 = !DISubroutineType(types: !368)
!368 = !{!360, !350, !369}
!369 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !370, file: !112, line: 1975, baseType: !131)
!370 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<false, std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > &&, std::__nonesuch_no_braces &&>", scope: !13, file: !112, line: 1974, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !371, identifier: "_ZTSSt11conditionalILb0EOSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EOSt20__nonesuch_no_bracesE")
!371 = !{!364, !372, !130}
!372 = !DITemplateTypeParameter(name: "_Iftrue", type: !356)
!373 = !DISubprogram(name: "swap", linkageName: "_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_E4swapERS7_", scope: !322, file: !72, line: 424, type: !374, isLocal: false, isDefinition: false, scopeLine: 424, flags: DIFlagPrototyped, isOptimized: true)
!374 = !DISubroutineType(types: !375)
!375 = !{null, !350, !360}
!376 = !{!377, !378}
!377 = !DITemplateTypeParameter(name: "_T1", type: !343)
!378 = !DITemplateTypeParameter(name: "_T2", type: !31)
!379 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !380, size: 64)
!380 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !315, file: !64, line: 387, baseType: !381)
!381 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !141, line: 108, size: 8, flags: DIFlagTypePassByReference, elements: !382, templateParams: !418, identifier: "_ZTSSaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_EE")
!382 = !{!383, !420, !424, !429}
!383 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !381, baseType: !384, flags: DIFlagPublic, extraData: i32 0)
!384 = !DIDerivedType(tag: DW_TAG_typedef, name: "__allocator_base<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !145, line: 48, baseType: !385)
!385 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "new_allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !5, file: !147, line: 58, size: 8, flags: DIFlagTypePassByReference, elements: !386, templateParams: !418, identifier: "_ZTSN9__gnu_cxx13new_allocatorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEE")
!386 = !{!387, !391, !396, !397, !403, !409, !412, !415}
!387 = !DISubprogram(name: "new_allocator", scope: !385, file: !147, line: 79, type: !388, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!388 = !DISubroutineType(types: !389)
!389 = !{null, !390}
!390 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !385, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!391 = !DISubprogram(name: "new_allocator", scope: !385, file: !147, line: 81, type: !392, isLocal: false, isDefinition: false, scopeLine: 81, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!392 = !DISubroutineType(types: !393)
!393 = !{null, !390, !394}
!394 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !395, size: 64)
!395 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !385)
!396 = !DISubprogram(name: "~new_allocator", scope: !385, file: !147, line: 86, type: !388, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!397 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE7addressERS9_", scope: !385, file: !147, line: 89, type: !398, isLocal: false, isDefinition: false, scopeLine: 89, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!398 = !DISubroutineType(types: !399)
!399 = !{!400, !401, !402}
!400 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !385, file: !147, line: 63, baseType: !321)
!401 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !395, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!402 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !385, file: !147, line: 65, baseType: !360)
!403 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE7addressERKS9_", scope: !385, file: !147, line: 93, type: !404, isLocal: false, isDefinition: false, scopeLine: 93, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!404 = !DISubroutineType(types: !405)
!405 = !{!406, !401, !408}
!406 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_pointer", scope: !385, file: !147, line: 64, baseType: !407)
!407 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !352, size: 64)
!408 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !385, file: !147, line: 66, baseType: !351)
!409 = !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE8allocateEmPKv", scope: !385, file: !147, line: 99, type: !410, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!410 = !DISubroutineType(types: !411)
!411 = !{!400, !390, !174, !178}
!412 = !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE10deallocateEPS9_m", scope: !385, file: !147, line: 116, type: !413, isLocal: false, isDefinition: false, scopeLine: 116, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!413 = !DISubroutineType(types: !414)
!414 = !{null, !390, !400, !174}
!415 = !DISubprogram(name: "max_size", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE8max_sizeEv", scope: !385, file: !147, line: 129, type: !416, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!416 = !DISubroutineType(types: !417)
!417 = !{!174, !401}
!418 = !{!419}
!419 = !DITemplateTypeParameter(name: "_Tp", type: !322)
!420 = !DISubprogram(name: "allocator", scope: !381, file: !141, line: 131, type: !421, isLocal: false, isDefinition: false, scopeLine: 131, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!421 = !DISubroutineType(types: !422)
!422 = !{null, !423}
!423 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !381, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!424 = !DISubprogram(name: "allocator", scope: !381, file: !141, line: 133, type: !425, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!425 = !DISubroutineType(types: !426)
!426 = !{null, !423, !427}
!427 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !428, size: 64)
!428 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !381)
!429 = !DISubprogram(name: "~allocator", scope: !381, file: !141, line: 139, type: !421, isLocal: false, isDefinition: false, scopeLine: 139, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!430 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE8allocateERS9_mPKv", scope: !315, file: !64, line: 449, type: !431, isLocal: false, isDefinition: false, scopeLine: 449, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!431 = !DISubroutineType(types: !432)
!432 = !{!320, !379, !198, !202}
!433 = !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE10deallocateERS9_PS8_m", scope: !315, file: !64, line: 461, type: !434, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!434 = !DISubroutineType(types: !435)
!435 = !{null, !379, !320, !198}
!436 = !DISubprogram(name: "max_size", linkageName: "_ZNSt16allocator_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE8max_sizeERKS9_", scope: !315, file: !64, line: 495, type: !437, isLocal: false, isDefinition: false, scopeLine: 495, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!437 = !DISubroutineType(types: !438)
!438 = !{!439, !440}
!439 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !315, file: !64, line: 407, baseType: !175)
!440 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !441, size: 64)
!441 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !380)
!442 = !DISubprogram(name: "select_on_container_copy_construction", linkageName: "_ZNSt16allocator_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE37select_on_container_copy_constructionERKS9_", scope: !315, file: !64, line: 504, type: !443, isLocal: false, isDefinition: false, scopeLine: 504, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!443 = !DISubroutineType(types: !444)
!444 = !{!380, !440}
!445 = !{!446}
!446 = !DITemplateTypeParameter(name: "_Alloc", type: !381)
!447 = !DISubprogram(name: "_S_select_on_copy", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E17_S_select_on_copyERKSA_", scope: !312, file: !59, line: 94, type: !448, isLocal: false, isDefinition: false, scopeLine: 94, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!448 = !DISubroutineType(types: !449)
!449 = !{!381, !427}
!450 = !DISubprogram(name: "_S_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E10_S_on_swapERSA_SC_", scope: !312, file: !59, line: 97, type: !451, isLocal: false, isDefinition: false, scopeLine: 97, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!451 = !DISubroutineType(types: !452)
!452 = !{null, !453, !453}
!453 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !381, size: 64)
!454 = !DISubprogram(name: "_S_propagate_on_copy_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E27_S_propagate_on_copy_assignEv", scope: !312, file: !59, line: 100, type: !225, isLocal: false, isDefinition: false, scopeLine: 100, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!455 = !DISubprogram(name: "_S_propagate_on_move_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E27_S_propagate_on_move_assignEv", scope: !312, file: !59, line: 103, type: !225, isLocal: false, isDefinition: false, scopeLine: 103, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!456 = !DISubprogram(name: "_S_propagate_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E20_S_propagate_on_swapEv", scope: !312, file: !59, line: 106, type: !225, isLocal: false, isDefinition: false, scopeLine: 106, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!457 = !DISubprogram(name: "_S_always_equal", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E15_S_always_equalEv", scope: !312, file: !59, line: 109, type: !225, isLocal: false, isDefinition: false, scopeLine: 109, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!458 = !DISubprogram(name: "_S_nothrow_move", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EES9_E15_S_nothrow_moveEv", scope: !312, file: !59, line: 112, type: !225, isLocal: false, isDefinition: false, scopeLine: 112, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!459 = !{!446, !460}
!460 = !DITemplateTypeParameter(type: !322)
!461 = !{!462}
!462 = !DITemplateTypeParameter(name: "_Tp", type: !463)
!463 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !12, line: 216, size: 768, flags: DIFlagTypePassByValue, elements: !464, templateParams: !527, identifier: "_ZTSSt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE")
!464 = !{!465, !485, !518, !522}
!465 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !463, baseType: !466, extraData: i32 0)
!466 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_node_base", scope: !13, file: !12, line: 101, size: 256, flags: DIFlagTypePassByValue, elements: !467, identifier: "_ZTSSt18_Rb_tree_node_base")
!467 = !{!468, !469, !472, !473, !474, !477, !483, !484}
!468 = !DIDerivedType(tag: DW_TAG_member, name: "_M_color", scope: !466, file: !12, line: 106, baseType: !11, size: 32)
!469 = !DIDerivedType(tag: DW_TAG_member, name: "_M_parent", scope: !466, file: !12, line: 107, baseType: !470, size: 64, offset: 64)
!470 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Base_ptr", scope: !466, file: !12, line: 103, baseType: !471)
!471 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !466, size: 64)
!472 = !DIDerivedType(tag: DW_TAG_member, name: "_M_left", scope: !466, file: !12, line: 108, baseType: !470, size: 64, offset: 128)
!473 = !DIDerivedType(tag: DW_TAG_member, name: "_M_right", scope: !466, file: !12, line: 109, baseType: !470, size: 64, offset: 192)
!474 = !DISubprogram(name: "_S_minimum", linkageName: "_ZNSt18_Rb_tree_node_base10_S_minimumEPS_", scope: !466, file: !12, line: 112, type: !475, isLocal: false, isDefinition: false, scopeLine: 112, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!475 = !DISubroutineType(types: !476)
!476 = !{!470, !470}
!477 = !DISubprogram(name: "_S_minimum", linkageName: "_ZNSt18_Rb_tree_node_base10_S_minimumEPKS_", scope: !466, file: !12, line: 119, type: !478, isLocal: false, isDefinition: false, scopeLine: 119, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!478 = !DISubroutineType(types: !479)
!479 = !{!480, !480}
!480 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Const_Base_ptr", scope: !466, file: !12, line: 104, baseType: !481)
!481 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !482, size: 64)
!482 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !466)
!483 = !DISubprogram(name: "_S_maximum", linkageName: "_ZNSt18_Rb_tree_node_base10_S_maximumEPS_", scope: !466, file: !12, line: 126, type: !475, isLocal: false, isDefinition: false, scopeLine: 126, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!484 = !DISubprogram(name: "_S_maximum", linkageName: "_ZNSt18_Rb_tree_node_base10_S_maximumEPKS_", scope: !466, file: !12, line: 133, type: !478, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!485 = !DIDerivedType(tag: DW_TAG_member, name: "_M_storage", scope: !463, file: !12, line: 231, baseType: !486, size: 512, offset: 256)
!486 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__aligned_membuf<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !5, file: !487, line: 47, size: 512, flags: DIFlagTypePassByValue, elements: !488, templateParams: !418, identifier: "_ZTSN9__gnu_cxx16__aligned_membufISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEE")
!487 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/ext/aligned_buffer.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!488 = !{!489, !494, !498, !503, !507, !512, !515}
!489 = !DIDerivedType(tag: DW_TAG_member, name: "_M_storage", scope: !486, file: !487, line: 54, baseType: !490, size: 512, align: 64)
!490 = !DICompositeType(tag: DW_TAG_array_type, baseType: !491, size: 512, elements: !492)
!491 = !DIBasicType(name: "unsigned char", size: 8, encoding: DW_ATE_unsigned_char)
!492 = !{!493}
!493 = !DISubrange(count: 64)
!494 = !DISubprogram(name: "__aligned_membuf", scope: !486, file: !487, line: 56, type: !495, isLocal: false, isDefinition: false, scopeLine: 56, flags: DIFlagPrototyped, isOptimized: true)
!495 = !DISubroutineType(types: !496)
!496 = !{null, !497}
!497 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !486, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!498 = !DISubprogram(name: "__aligned_membuf", scope: !486, file: !487, line: 59, type: !499, isLocal: false, isDefinition: false, scopeLine: 59, flags: DIFlagPrototyped, isOptimized: true)
!499 = !DISubroutineType(types: !500)
!500 = !{null, !497, !501}
!501 = !DIDerivedType(tag: DW_TAG_typedef, name: "nullptr_t", scope: !13, file: !176, line: 242, baseType: !502)
!502 = !DIBasicType(tag: DW_TAG_unspecified_type, name: "decltype(nullptr)")
!503 = !DISubprogram(name: "_M_addr", linkageName: "_ZN9__gnu_cxx16__aligned_membufISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE7_M_addrEv", scope: !486, file: !487, line: 62, type: !504, isLocal: false, isDefinition: false, scopeLine: 62, flags: DIFlagPrototyped, isOptimized: true)
!504 = !DISubroutineType(types: !505)
!505 = !{!506, !497}
!506 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: null, size: 64)
!507 = !DISubprogram(name: "_M_addr", linkageName: "_ZNK9__gnu_cxx16__aligned_membufISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE7_M_addrEv", scope: !486, file: !487, line: 66, type: !508, isLocal: false, isDefinition: false, scopeLine: 66, flags: DIFlagPrototyped, isOptimized: true)
!508 = !DISubroutineType(types: !509)
!509 = !{!178, !510}
!510 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !511, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!511 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !486)
!512 = !DISubprogram(name: "_M_ptr", linkageName: "_ZN9__gnu_cxx16__aligned_membufISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE6_M_ptrEv", scope: !486, file: !487, line: 70, type: !513, isLocal: false, isDefinition: false, scopeLine: 70, flags: DIFlagPrototyped, isOptimized: true)
!513 = !DISubroutineType(types: !514)
!514 = !{!321, !497}
!515 = !DISubprogram(name: "_M_ptr", linkageName: "_ZNK9__gnu_cxx16__aligned_membufISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE6_M_ptrEv", scope: !486, file: !487, line: 74, type: !516, isLocal: false, isDefinition: false, scopeLine: 74, flags: DIFlagPrototyped, isOptimized: true)
!516 = !DISubroutineType(types: !517)
!517 = !{!407, !510}
!518 = !DISubprogram(name: "_M_valptr", linkageName: "_ZNSt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE9_M_valptrEv", scope: !463, file: !12, line: 234, type: !519, isLocal: false, isDefinition: false, scopeLine: 234, flags: DIFlagPrototyped, isOptimized: true)
!519 = !DISubroutineType(types: !520)
!520 = !{!321, !521}
!521 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !463, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!522 = !DISubprogram(name: "_M_valptr", linkageName: "_ZNKSt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE9_M_valptrEv", scope: !463, file: !12, line: 238, type: !523, isLocal: false, isDefinition: false, scopeLine: 238, flags: DIFlagPrototyped, isOptimized: true)
!523 = !DISubroutineType(types: !524)
!524 = !{!407, !525}
!525 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !526, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!526 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !463)
!527 = !{!528}
!528 = !DITemplateTypeParameter(name: "_Val", type: !322)
!529 = !DIDerivedType(tag: DW_TAG_typedef, name: "rebind_alloc<std::_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !315, file: !64, line: 422, baseType: !530)
!530 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "allocator<std::_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !141, line: 108, size: 8, flags: DIFlagTypePassByReference, elements: !531, templateParams: !580, identifier: "_ZTSSaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEE")
!531 = !{!532, !570, !574, !579}
!532 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !530, baseType: !533, flags: DIFlagPublic, extraData: i32 0)
!533 = !DIDerivedType(tag: DW_TAG_typedef, name: "__allocator_base<std::_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !145, line: 48, baseType: !534)
!534 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "new_allocator<std::_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !5, file: !147, line: 58, size: 8, flags: DIFlagTypePassByReference, elements: !535, templateParams: !461, identifier: "_ZTSN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEEE")
!535 = !{!536, !540, !545, !546, !554, !561, !564, !567}
!536 = !DISubprogram(name: "new_allocator", scope: !534, file: !147, line: 79, type: !537, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!537 = !DISubroutineType(types: !538)
!538 = !{null, !539}
!539 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !534, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!540 = !DISubprogram(name: "new_allocator", scope: !534, file: !147, line: 81, type: !541, isLocal: false, isDefinition: false, scopeLine: 81, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!541 = !DISubroutineType(types: !542)
!542 = !{null, !539, !543}
!543 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !544, size: 64)
!544 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !534)
!545 = !DISubprogram(name: "~new_allocator", scope: !534, file: !147, line: 86, type: !537, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!546 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE7addressERSB_", scope: !534, file: !147, line: 89, type: !547, isLocal: false, isDefinition: false, scopeLine: 89, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!547 = !DISubroutineType(types: !548)
!548 = !{!549, !551, !552}
!549 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !534, file: !147, line: 63, baseType: !550)
!550 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !463, size: 64)
!551 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !544, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!552 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !534, file: !147, line: 65, baseType: !553)
!553 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !463, size: 64)
!554 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE7addressERKSB_", scope: !534, file: !147, line: 93, type: !555, isLocal: false, isDefinition: false, scopeLine: 93, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!555 = !DISubroutineType(types: !556)
!556 = !{!557, !551, !559}
!557 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_pointer", scope: !534, file: !147, line: 64, baseType: !558)
!558 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !526, size: 64)
!559 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !534, file: !147, line: 66, baseType: !560)
!560 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !526, size: 64)
!561 = !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE8allocateEmPKv", scope: !534, file: !147, line: 99, type: !562, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!562 = !DISubroutineType(types: !563)
!563 = !{!549, !539, !174, !178}
!564 = !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE10deallocateEPSB_m", scope: !534, file: !147, line: 116, type: !565, isLocal: false, isDefinition: false, scopeLine: 116, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!565 = !DISubroutineType(types: !566)
!566 = !{null, !539, !549, !174}
!567 = !DISubprogram(name: "max_size", linkageName: "_ZNK9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE8max_sizeEv", scope: !534, file: !147, line: 129, type: !568, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!568 = !DISubroutineType(types: !569)
!569 = !{!174, !551}
!570 = !DISubprogram(name: "allocator", scope: !530, file: !141, line: 131, type: !571, isLocal: false, isDefinition: false, scopeLine: 131, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!571 = !DISubroutineType(types: !572)
!572 = !{null, !573}
!573 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !530, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!574 = !DISubprogram(name: "allocator", scope: !530, file: !141, line: 133, type: !575, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!575 = !DISubroutineType(types: !576)
!576 = !{null, !573, !577}
!577 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !578, size: 64)
!578 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !530)
!579 = !DISubprogram(name: "~allocator", scope: !530, file: !141, line: 139, type: !571, isLocal: false, isDefinition: false, scopeLine: 139, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!580 = !{!581}
!581 = !DITemplateTypeParameter(type: !463)
!582 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !306, baseType: !583, extraData: i32 0)
!583 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_key_compare<std::less<std::__cxx11::basic_string<char> > >", scope: !13, file: !12, line: 142, size: 8, flags: DIFlagTypePassByReference, elements: !584, templateParams: !620, identifier: "_ZTSSt20_Rb_tree_key_compareISt4lessINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE")
!584 = !{!585, !603, !607, !611, !616}
!585 = !DIDerivedType(tag: DW_TAG_member, name: "_M_key_compare", scope: !583, file: !12, line: 144, baseType: !586, size: 8)
!586 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "less<std::__cxx11::basic_string<char> >", scope: !13, file: !587, line: 381, size: 8, flags: DIFlagTypePassByValue, elements: !588, templateParams: !601, identifier: "_ZTSSt4lessINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE")
!587 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_function.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!588 = !{!589, !595}
!589 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !586, baseType: !590, extraData: i32 0)
!590 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "binary_function<std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char>, bool>", scope: !13, file: !587, line: 118, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !591, identifier: "_ZTSSt15binary_functionINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_bE")
!591 = !{!592, !593, !594}
!592 = !DITemplateTypeParameter(name: "_Arg1", type: !31)
!593 = !DITemplateTypeParameter(name: "_Arg2", type: !31)
!594 = !DITemplateTypeParameter(name: "_Result", type: !117)
!595 = !DISubprogram(name: "operator()", linkageName: "_ZNKSt4lessINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEclERKS5_S8_", scope: !586, file: !587, line: 385, type: !596, isLocal: false, isDefinition: false, scopeLine: 385, flags: DIFlagPrototyped, isOptimized: true)
!596 = !DISubroutineType(types: !597)
!597 = !{!117, !598, !600, !600}
!598 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !599, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!599 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !586)
!600 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !343, size: 64)
!601 = !{!602}
!602 = !DITemplateTypeParameter(name: "_Tp", type: !31)
!603 = !DISubprogram(name: "_Rb_tree_key_compare", scope: !583, file: !12, line: 146, type: !604, isLocal: false, isDefinition: false, scopeLine: 146, flags: DIFlagPrototyped, isOptimized: true)
!604 = !DISubroutineType(types: !605)
!605 = !{null, !606}
!606 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !583, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!607 = !DISubprogram(name: "_Rb_tree_key_compare", scope: !583, file: !12, line: 152, type: !608, isLocal: false, isDefinition: false, scopeLine: 152, flags: DIFlagPrototyped, isOptimized: true)
!608 = !DISubroutineType(types: !609)
!609 = !{null, !606, !610}
!610 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !599, size: 64)
!611 = !DISubprogram(name: "_Rb_tree_key_compare", scope: !583, file: !12, line: 158, type: !612, isLocal: false, isDefinition: false, scopeLine: 158, flags: DIFlagPrototyped, isOptimized: true)
!612 = !DISubroutineType(types: !613)
!613 = !{null, !606, !614}
!614 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !615, size: 64)
!615 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !583)
!616 = !DISubprogram(name: "_Rb_tree_key_compare", scope: !583, file: !12, line: 160, type: !617, isLocal: false, isDefinition: false, scopeLine: 160, flags: DIFlagPrototyped, isOptimized: true)
!617 = !DISubroutineType(types: !618)
!618 = !{null, !606, !619}
!619 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !583, size: 64)
!620 = !{!621}
!621 = !DITemplateTypeParameter(name: "_Key_compare", type: !586)
!622 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !306, baseType: !623, offset: 64, extraData: i32 0)
!623 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_header", scope: !13, file: !12, line: 168, size: 320, flags: DIFlagTypePassByReference, elements: !624, identifier: "_ZTSSt15_Rb_tree_header")
!624 = !{!625, !626, !627, !631, !635, !639}
!625 = !DIDerivedType(tag: DW_TAG_member, name: "_M_header", scope: !623, file: !12, line: 170, baseType: !466, size: 256)
!626 = !DIDerivedType(tag: DW_TAG_member, name: "_M_node_count", scope: !623, file: !12, line: 171, baseType: !175, size: 64, offset: 256)
!627 = !DISubprogram(name: "_Rb_tree_header", scope: !623, file: !12, line: 173, type: !628, isLocal: false, isDefinition: false, scopeLine: 173, flags: DIFlagPrototyped, isOptimized: true)
!628 = !DISubroutineType(types: !629)
!629 = !{null, !630}
!630 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !623, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!631 = !DISubprogram(name: "_Rb_tree_header", scope: !623, file: !12, line: 180, type: !632, isLocal: false, isDefinition: false, scopeLine: 180, flags: DIFlagPrototyped, isOptimized: true)
!632 = !DISubroutineType(types: !633)
!633 = !{null, !630, !634}
!634 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !623, size: 64)
!635 = !DISubprogram(name: "_M_move_data", linkageName: "_ZNSt15_Rb_tree_header12_M_move_dataERS_", scope: !623, file: !12, line: 193, type: !636, isLocal: false, isDefinition: false, scopeLine: 193, flags: DIFlagPrototyped, isOptimized: true)
!636 = !DISubroutineType(types: !637)
!637 = !{null, !630, !638}
!638 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !623, size: 64)
!639 = !DISubprogram(name: "_M_reset", linkageName: "_ZNSt15_Rb_tree_header8_M_resetEv", scope: !623, file: !12, line: 206, type: !628, isLocal: false, isDefinition: false, scopeLine: 206, flags: DIFlagPrototyped, isOptimized: true)
!640 = !DISubprogram(name: "_Rb_tree_impl", scope: !306, file: !12, line: 699, type: !641, isLocal: false, isDefinition: false, scopeLine: 699, flags: DIFlagPrototyped, isOptimized: true)
!641 = !DISubroutineType(types: !642)
!642 = !{null, !643}
!643 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !306, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!644 = !DISubprogram(name: "_Rb_tree_impl", scope: !306, file: !12, line: 706, type: !645, isLocal: false, isDefinition: false, scopeLine: 706, flags: DIFlagPrototyped, isOptimized: true)
!645 = !DISubroutineType(types: !646)
!646 = !{null, !643, !647}
!647 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !648, size: 64)
!648 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !306)
!649 = !DISubprogram(name: "_Rb_tree_impl", scope: !306, file: !12, line: 716, type: !650, isLocal: false, isDefinition: false, scopeLine: 716, flags: DIFlagPrototyped, isOptimized: true)
!650 = !DISubroutineType(types: !651)
!651 = !{null, !643, !652}
!652 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !306, size: 64)
!653 = !DISubprogram(name: "_Rb_tree_impl", scope: !306, file: !12, line: 718, type: !654, isLocal: false, isDefinition: false, scopeLine: 718, flags: DIFlagPrototyped, isOptimized: true)
!654 = !DISubroutineType(types: !655)
!655 = !{null, !643, !610, !656}
!656 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !309, size: 64)
!657 = !{!621, !658}
!658 = !DITemplateValueParameter(type: !117, value: i8 1)
!659 = !DISubprogram(name: "_M_get_Node_allocator", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE21_M_get_Node_allocatorEv", scope: !303, file: !12, line: 585, type: !660, isLocal: false, isDefinition: false, scopeLine: 585, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!660 = !DISubroutineType(types: !661)
!661 = !{!662, !663}
!662 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !309, size: 64)
!663 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !303, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!664 = !DISubprogram(name: "_M_get_Node_allocator", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE21_M_get_Node_allocatorEv", scope: !303, file: !12, line: 589, type: !665, isLocal: false, isDefinition: false, scopeLine: 589, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!665 = !DISubroutineType(types: !666)
!666 = !{!667, !669}
!667 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !668, size: 64)
!668 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !309)
!669 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !670, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!670 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !303)
!671 = !DISubprogram(name: "get_allocator", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE13get_allocatorEv", scope: !303, file: !12, line: 593, type: !672, isLocal: false, isDefinition: false, scopeLine: 593, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!672 = !DISubroutineType(types: !673)
!673 = !{!674, !669}
!674 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !303, file: !12, line: 582, baseType: !381)
!675 = !DISubprogram(name: "_M_get_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11_M_get_nodeEv", scope: !303, file: !12, line: 598, type: !676, isLocal: false, isDefinition: false, scopeLine: 598, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!676 = !DISubroutineType(types: !677)
!677 = !{!302, !663}
!678 = !DISubprogram(name: "_M_put_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11_M_put_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 602, type: !679, isLocal: false, isDefinition: false, scopeLine: 602, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!679 = !DISubroutineType(types: !680)
!680 = !{null, !663, !302}
!681 = !DISubprogram(name: "_M_destroy_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE15_M_destroy_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 659, type: !679, isLocal: false, isDefinition: false, scopeLine: 659, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!682 = !DISubprogram(name: "_M_drop_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_drop_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 667, type: !679, isLocal: false, isDefinition: false, scopeLine: 667, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!683 = !DISubprogram(name: "_M_root", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE7_M_rootEv", scope: !303, file: !12, line: 728, type: !684, isLocal: false, isDefinition: false, scopeLine: 728, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!684 = !DISubroutineType(types: !685)
!685 = !{!686, !663}
!686 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !687, size: 64)
!687 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Base_ptr", file: !12, line: 463, baseType: !471)
!688 = !DISubprogram(name: "_M_root", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE7_M_rootEv", scope: !303, file: !12, line: 732, type: !689, isLocal: false, isDefinition: false, scopeLine: 732, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!689 = !DISubroutineType(types: !690)
!690 = !{!691, !669}
!691 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Const_Base_ptr", file: !12, line: 464, baseType: !481)
!692 = !DISubprogram(name: "_M_leftmost", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11_M_leftmostEv", scope: !303, file: !12, line: 736, type: !684, isLocal: false, isDefinition: false, scopeLine: 736, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!693 = !DISubprogram(name: "_M_leftmost", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11_M_leftmostEv", scope: !303, file: !12, line: 740, type: !689, isLocal: false, isDefinition: false, scopeLine: 740, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!694 = !DISubprogram(name: "_M_rightmost", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_rightmostEv", scope: !303, file: !12, line: 744, type: !684, isLocal: false, isDefinition: false, scopeLine: 744, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!695 = !DISubprogram(name: "_M_rightmost", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_rightmostEv", scope: !303, file: !12, line: 748, type: !689, isLocal: false, isDefinition: false, scopeLine: 748, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!696 = !DISubprogram(name: "_M_begin", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_beginEv", scope: !303, file: !12, line: 752, type: !676, isLocal: false, isDefinition: false, scopeLine: 752, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!697 = !DISubprogram(name: "_M_begin", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_beginEv", scope: !303, file: !12, line: 756, type: !698, isLocal: false, isDefinition: false, scopeLine: 756, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!698 = !DISubroutineType(types: !699)
!699 = !{!700, !669}
!700 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Const_Link_type", scope: !303, file: !12, line: 466, baseType: !558)
!701 = !DISubprogram(name: "_M_end", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE6_M_endEv", scope: !303, file: !12, line: 763, type: !702, isLocal: false, isDefinition: false, scopeLine: 763, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!702 = !DISubroutineType(types: !703)
!703 = !{!687, !663}
!704 = !DISubprogram(name: "_M_end", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE6_M_endEv", scope: !303, file: !12, line: 767, type: !689, isLocal: false, isDefinition: false, scopeLine: 767, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!705 = !DISubprogram(name: "_S_value", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_S_valueEPKSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 771, type: !706, isLocal: false, isDefinition: false, scopeLine: 771, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!706 = !DISubroutineType(types: !707)
!707 = !{!708, !700}
!708 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !303, file: !12, line: 579, baseType: !709)
!709 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !710, size: 64)
!710 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !711)
!711 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !303, file: !12, line: 575, baseType: !322)
!712 = !DISubprogram(name: "_S_key", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE6_S_keyEPKSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 775, type: !713, isLocal: false, isDefinition: false, scopeLine: 775, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!713 = !DISubroutineType(types: !714)
!714 = !{!600, !700}
!715 = !DISubprogram(name: "_S_left", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE7_S_leftEPSt18_Rb_tree_node_base", scope: !303, file: !12, line: 779, type: !716, isLocal: false, isDefinition: false, scopeLine: 779, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!716 = !DISubroutineType(types: !717)
!717 = !{!302, !687}
!718 = !DISubprogram(name: "_S_left", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE7_S_leftEPKSt18_Rb_tree_node_base", scope: !303, file: !12, line: 783, type: !719, isLocal: false, isDefinition: false, scopeLine: 783, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!719 = !DISubroutineType(types: !720)
!720 = !{!700, !691}
!721 = !DISubprogram(name: "_S_right", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_S_rightEPSt18_Rb_tree_node_base", scope: !303, file: !12, line: 787, type: !716, isLocal: false, isDefinition: false, scopeLine: 787, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!722 = !DISubprogram(name: "_S_right", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_S_rightEPKSt18_Rb_tree_node_base", scope: !303, file: !12, line: 791, type: !719, isLocal: false, isDefinition: false, scopeLine: 791, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!723 = !DISubprogram(name: "_S_value", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_S_valueEPKSt18_Rb_tree_node_base", scope: !303, file: !12, line: 795, type: !724, isLocal: false, isDefinition: false, scopeLine: 795, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!724 = !DISubroutineType(types: !725)
!725 = !{!708, !691}
!726 = !DISubprogram(name: "_S_key", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE6_S_keyEPKSt18_Rb_tree_node_base", scope: !303, file: !12, line: 799, type: !727, isLocal: false, isDefinition: false, scopeLine: 799, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!727 = !DISubroutineType(types: !728)
!728 = !{!600, !691}
!729 = !DISubprogram(name: "_S_minimum", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE10_S_minimumEPSt18_Rb_tree_node_base", scope: !303, file: !12, line: 803, type: !730, isLocal: false, isDefinition: false, scopeLine: 803, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!730 = !DISubroutineType(types: !731)
!731 = !{!732, !687}
!732 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Base_ptr", scope: !303, file: !12, line: 463, baseType: !471)
!733 = !DISubprogram(name: "_S_minimum", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE10_S_minimumEPKSt18_Rb_tree_node_base", scope: !303, file: !12, line: 807, type: !734, isLocal: false, isDefinition: false, scopeLine: 807, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!734 = !DISubroutineType(types: !735)
!735 = !{!736, !691}
!736 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Const_Base_ptr", scope: !303, file: !12, line: 464, baseType: !481)
!737 = !DISubprogram(name: "_S_maximum", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE10_S_maximumEPSt18_Rb_tree_node_base", scope: !303, file: !12, line: 811, type: !730, isLocal: false, isDefinition: false, scopeLine: 811, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!738 = !DISubprogram(name: "_S_maximum", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE10_S_maximumEPKSt18_Rb_tree_node_base", scope: !303, file: !12, line: 815, type: !734, isLocal: false, isDefinition: false, scopeLine: 815, flags: DIFlagProtected | DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!739 = !DISubprogram(name: "_M_get_insert_unique_pos", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE24_M_get_insert_unique_posERS7_", scope: !303, file: !12, line: 833, type: !740, isLocal: false, isDefinition: false, scopeLine: 833, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!740 = !DISubroutineType(types: !741)
!741 = !{!742, !663, !797}
!742 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "pair<std::_Rb_tree_node_base *, std::_Rb_tree_node_base *>", scope: !13, file: !72, line: 208, size: 128, flags: DIFlagTypePassByValue, elements: !743, templateParams: !794, identifier: "_ZTSSt4pairIPSt18_Rb_tree_node_baseS1_E")
!743 = !{!744, !764, !765, !766, !772, !776, !784, !791}
!744 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !742, baseType: !745, flags: DIFlagPrivate, extraData: i32 0)
!745 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "__pair_base<std::_Rb_tree_node_base *, std::_Rb_tree_node_base *>", scope: !13, file: !72, line: 190, size: 8, flags: DIFlagTypePassByValue, elements: !746, templateParams: !761, identifier: "_ZTSSt11__pair_baseIPSt18_Rb_tree_node_baseS1_E")
!746 = !{!747, !751, !752, !757}
!747 = !DISubprogram(name: "__pair_base", scope: !745, file: !72, line: 194, type: !748, isLocal: false, isDefinition: false, scopeLine: 194, flags: DIFlagPrototyped, isOptimized: true)
!748 = !DISubroutineType(types: !749)
!749 = !{null, !750}
!750 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !745, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!751 = !DISubprogram(name: "~__pair_base", scope: !745, file: !72, line: 195, type: !748, isLocal: false, isDefinition: false, scopeLine: 195, flags: DIFlagPrototyped, isOptimized: true)
!752 = !DISubprogram(name: "__pair_base", scope: !745, file: !72, line: 196, type: !753, isLocal: false, isDefinition: false, scopeLine: 196, flags: DIFlagPrototyped, isOptimized: true)
!753 = !DISubroutineType(types: !754)
!754 = !{null, !750, !755}
!755 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !756, size: 64)
!756 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !745)
!757 = !DISubprogram(name: "operator=", linkageName: "_ZNSt11__pair_baseIPSt18_Rb_tree_node_baseS1_EaSERKS2_", scope: !745, file: !72, line: 197, type: !758, isLocal: false, isDefinition: false, scopeLine: 197, flags: DIFlagPrototyped, isOptimized: true)
!758 = !DISubroutineType(types: !759)
!759 = !{!760, !750, !755}
!760 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !745, size: 64)
!761 = !{!762, !763}
!762 = !DITemplateTypeParameter(name: "_U1", type: !471)
!763 = !DITemplateTypeParameter(name: "_U2", type: !471)
!764 = !DIDerivedType(tag: DW_TAG_member, name: "first", scope: !742, file: !72, line: 214, baseType: !471, size: 64)
!765 = !DIDerivedType(tag: DW_TAG_member, name: "second", scope: !742, file: !72, line: 215, baseType: !471, size: 64, offset: 64)
!766 = !DISubprogram(name: "pair", scope: !742, file: !72, line: 303, type: !767, isLocal: false, isDefinition: false, scopeLine: 303, flags: DIFlagPrototyped, isOptimized: true)
!767 = !DISubroutineType(types: !768)
!768 = !{null, !769, !770}
!769 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !742, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!770 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !771, size: 64)
!771 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !742)
!772 = !DISubprogram(name: "pair", scope: !742, file: !72, line: 304, type: !773, isLocal: false, isDefinition: false, scopeLine: 304, flags: DIFlagPrototyped, isOptimized: true)
!773 = !DISubroutineType(types: !774)
!774 = !{null, !769, !775}
!775 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !742, size: 64)
!776 = !DISubprogram(name: "operator=", linkageName: "_ZNSt4pairIPSt18_Rb_tree_node_baseS1_EaSERKS2_", scope: !742, file: !72, line: 378, type: !777, isLocal: false, isDefinition: false, scopeLine: 378, flags: DIFlagPrototyped, isOptimized: true)
!777 = !DISubroutineType(types: !778)
!778 = !{!779, !769, !780}
!779 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !742, size: 64)
!780 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !781, file: !112, line: 1970, baseType: !770)
!781 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<true, const std::pair<std::_Rb_tree_node_base *, std::_Rb_tree_node_base *> &, const std::__nonesuch_no_braces &>", scope: !13, file: !112, line: 1969, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !782, identifier: "_ZTSSt11conditionalILb1ERKSt4pairIPSt18_Rb_tree_node_baseS2_ERKSt20__nonesuch_no_bracesE")
!782 = !{!116, !783, !119}
!783 = !DITemplateTypeParameter(name: "_Iftrue", type: !770)
!784 = !DISubprogram(name: "operator=", linkageName: "_ZNSt4pairIPSt18_Rb_tree_node_baseS1_EaSEOS2_", scope: !742, file: !72, line: 389, type: !785, isLocal: false, isDefinition: false, scopeLine: 389, flags: DIFlagPrototyped, isOptimized: true)
!785 = !DISubroutineType(types: !786)
!786 = !{!779, !769, !787}
!787 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !788, file: !112, line: 1970, baseType: !775)
!788 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<true, std::pair<std::_Rb_tree_node_base *, std::_Rb_tree_node_base *> &&, std::__nonesuch_no_braces &&>", scope: !13, file: !112, line: 1969, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !789, identifier: "_ZTSSt11conditionalILb1EOSt4pairIPSt18_Rb_tree_node_baseS2_EOSt20__nonesuch_no_bracesE")
!789 = !{!116, !790, !130}
!790 = !DITemplateTypeParameter(name: "_Iftrue", type: !775)
!791 = !DISubprogram(name: "swap", linkageName: "_ZNSt4pairIPSt18_Rb_tree_node_baseS1_E4swapERS2_", scope: !742, file: !72, line: 424, type: !792, isLocal: false, isDefinition: false, scopeLine: 424, flags: DIFlagPrototyped, isOptimized: true)
!792 = !DISubroutineType(types: !793)
!793 = !{null, !769, !779}
!794 = !{!795, !796}
!795 = !DITemplateTypeParameter(name: "_T1", type: !471)
!796 = !DITemplateTypeParameter(name: "_T2", type: !471)
!797 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !798, size: 64)
!798 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !799)
!799 = !DIDerivedType(tag: DW_TAG_typedef, name: "key_type", scope: !303, file: !12, line: 574, baseType: !31)
!800 = !DISubprogram(name: "_M_get_insert_equal_pos", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE23_M_get_insert_equal_posERS7_", scope: !303, file: !12, line: 836, type: !740, isLocal: false, isDefinition: false, scopeLine: 836, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!801 = !DISubprogram(name: "_M_get_insert_hint_unique_pos", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE29_M_get_insert_hint_unique_posESt23_Rb_tree_const_iteratorIS8_ERS7_", scope: !303, file: !12, line: 839, type: !802, isLocal: false, isDefinition: false, scopeLine: 839, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!802 = !DISubroutineType(types: !803)
!803 = !{!742, !663, !804, !797}
!804 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_iterator", scope: !303, file: !12, line: 820, baseType: !805)
!805 = !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_const_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !12, line: 326, flags: DIFlagFwdDecl, identifier: "_ZTSSt23_Rb_tree_const_iteratorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE")
!806 = !DISubprogram(name: "_M_get_insert_hint_equal_pos", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE28_M_get_insert_hint_equal_posESt23_Rb_tree_const_iteratorIS8_ERS7_", scope: !303, file: !12, line: 843, type: !802, isLocal: false, isDefinition: false, scopeLine: 843, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!807 = !DISubprogram(name: "_M_insert_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_insert_nodeEPSt18_Rb_tree_node_baseSG_PSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 853, type: !808, isLocal: false, isDefinition: false, scopeLine: 853, flags: DIFlagPrototyped, isOptimized: true)
!808 = !DISubroutineType(types: !809)
!809 = !{!810, !663, !687, !687, !302}
!810 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator", scope: !303, file: !12, line: 819, baseType: !811)
!811 = !DICompositeType(tag: DW_TAG_structure_type, name: "_Rb_tree_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !12, line: 256, flags: DIFlagFwdDecl, identifier: "_ZTSSt17_Rb_tree_iteratorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE")
!812 = !DISubprogram(name: "_M_insert_lower_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE20_M_insert_lower_nodeEPSt18_Rb_tree_node_basePSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 864, type: !813, isLocal: false, isDefinition: false, scopeLine: 864, flags: DIFlagPrototyped, isOptimized: true)
!813 = !DISubroutineType(types: !814)
!814 = !{!810, !663, !687, !302}
!815 = !DISubprogram(name: "_M_insert_equal_lower_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE26_M_insert_equal_lower_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 867, type: !816, isLocal: false, isDefinition: false, scopeLine: 867, flags: DIFlagPrototyped, isOptimized: true)
!816 = !DISubroutineType(types: !817)
!817 = !{!810, !663, !302}
!818 = !DISubprogram(name: "_M_copy", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE7_M_copyERKSE_", scope: !303, file: !12, line: 899, type: !819, isLocal: false, isDefinition: false, scopeLine: 899, flags: DIFlagPrototyped, isOptimized: true)
!819 = !DISubroutineType(types: !820)
!820 = !{!302, !663, !821}
!821 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !670, size: 64)
!822 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_eraseEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 906, type: !679, isLocal: false, isDefinition: false, scopeLine: 906, flags: DIFlagPrototyped, isOptimized: true)
!823 = !DISubprogram(name: "_M_lower_bound", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_lower_boundEPSt13_Rb_tree_nodeIS8_EPSt18_Rb_tree_node_baseRS7_", scope: !303, file: !12, line: 909, type: !824, isLocal: false, isDefinition: false, scopeLine: 909, flags: DIFlagPrototyped, isOptimized: true)
!824 = !DISubroutineType(types: !825)
!825 = !{!810, !663, !302, !687, !600}
!826 = !DISubprogram(name: "_M_lower_bound", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_lower_boundEPKSt13_Rb_tree_nodeIS8_EPKSt18_Rb_tree_node_baseRS7_", scope: !303, file: !12, line: 913, type: !827, isLocal: false, isDefinition: false, scopeLine: 913, flags: DIFlagPrototyped, isOptimized: true)
!827 = !DISubroutineType(types: !828)
!828 = !{!804, !669, !700, !691, !600}
!829 = !DISubprogram(name: "_M_upper_bound", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_upper_boundEPSt13_Rb_tree_nodeIS8_EPSt18_Rb_tree_node_baseRS7_", scope: !303, file: !12, line: 917, type: !824, isLocal: false, isDefinition: false, scopeLine: 917, flags: DIFlagPrototyped, isOptimized: true)
!830 = !DISubprogram(name: "_M_upper_bound", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_upper_boundEPKSt13_Rb_tree_nodeIS8_EPKSt18_Rb_tree_node_baseRS7_", scope: !303, file: !12, line: 921, type: !827, isLocal: false, isDefinition: false, scopeLine: 921, flags: DIFlagPrototyped, isOptimized: true)
!831 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 929, type: !832, isLocal: false, isDefinition: false, scopeLine: 929, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!832 = !DISubroutineType(types: !833)
!833 = !{null, !663}
!834 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 932, type: !835, isLocal: false, isDefinition: false, scopeLine: 932, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!835 = !DISubroutineType(types: !836)
!836 = !{null, !663, !610, !837}
!837 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !838, size: 64)
!838 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !674)
!839 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 936, type: !840, isLocal: false, isDefinition: false, scopeLine: 936, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!840 = !DISubroutineType(types: !841)
!841 = !{null, !663, !821}
!842 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 944, type: !843, isLocal: false, isDefinition: false, scopeLine: 944, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!843 = !DISubroutineType(types: !844)
!844 = !{null, !663, !837}
!845 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 948, type: !846, isLocal: false, isDefinition: false, scopeLine: 948, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!846 = !DISubroutineType(types: !847)
!847 = !{null, !663, !821, !837}
!848 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 955, type: !849, isLocal: false, isDefinition: false, scopeLine: 955, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!849 = !DISubroutineType(types: !850)
!850 = !{null, !663, !851}
!851 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !303, size: 64)
!852 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 957, type: !853, isLocal: false, isDefinition: false, scopeLine: 957, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!853 = !DISubroutineType(types: !854)
!854 = !{null, !663, !851, !837}
!855 = !DISubprogram(name: "_Rb_tree", scope: !303, file: !12, line: 961, type: !856, isLocal: false, isDefinition: false, scopeLine: 961, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!856 = !DISubroutineType(types: !857)
!857 = !{null, !663, !851, !656}
!858 = !DISubprogram(name: "~_Rb_tree", scope: !303, file: !12, line: 964, type: !832, isLocal: false, isDefinition: false, scopeLine: 964, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!859 = !DISubprogram(name: "operator=", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EEaSERKSE_", scope: !303, file: !12, line: 968, type: !860, isLocal: false, isDefinition: false, scopeLine: 968, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!860 = !DISubroutineType(types: !861)
!861 = !{!862, !663, !821}
!862 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !303, size: 64)
!863 = !DISubprogram(name: "key_comp", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8key_compEv", scope: !303, file: !12, line: 972, type: !864, isLocal: false, isDefinition: false, scopeLine: 972, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!864 = !DISubroutineType(types: !865)
!865 = !{!586, !669}
!866 = !DISubprogram(name: "begin", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5beginEv", scope: !303, file: !12, line: 976, type: !867, isLocal: false, isDefinition: false, scopeLine: 976, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!867 = !DISubroutineType(types: !868)
!868 = !{!810, !663}
!869 = !DISubprogram(name: "begin", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5beginEv", scope: !303, file: !12, line: 980, type: !870, isLocal: false, isDefinition: false, scopeLine: 980, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!870 = !DISubroutineType(types: !871)
!871 = !{!804, !669}
!872 = !DISubprogram(name: "end", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE3endEv", scope: !303, file: !12, line: 984, type: !867, isLocal: false, isDefinition: false, scopeLine: 984, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!873 = !DISubprogram(name: "end", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE3endEv", scope: !303, file: !12, line: 988, type: !870, isLocal: false, isDefinition: false, scopeLine: 988, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!874 = !DISubprogram(name: "rbegin", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE6rbeginEv", scope: !303, file: !12, line: 992, type: !875, isLocal: false, isDefinition: false, scopeLine: 992, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!875 = !DISubroutineType(types: !876)
!876 = !{!877, !663}
!877 = !DIDerivedType(tag: DW_TAG_typedef, name: "reverse_iterator", scope: !303, file: !12, line: 822, baseType: !878)
!878 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<std::_Rb_tree_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !879, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorISt17_Rb_tree_iteratorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEE")
!879 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_iterator.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!880 = !DISubprogram(name: "rbegin", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE6rbeginEv", scope: !303, file: !12, line: 996, type: !881, isLocal: false, isDefinition: false, scopeLine: 996, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!881 = !DISubroutineType(types: !882)
!882 = !{!883, !669}
!883 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reverse_iterator", scope: !303, file: !12, line: 823, baseType: !884)
!884 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<std::_Rb_tree_const_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !879, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorISt23_Rb_tree_const_iteratorISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEE")
!885 = !DISubprogram(name: "rend", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE4rendEv", scope: !303, file: !12, line: 1000, type: !875, isLocal: false, isDefinition: false, scopeLine: 1000, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!886 = !DISubprogram(name: "rend", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE4rendEv", scope: !303, file: !12, line: 1004, type: !881, isLocal: false, isDefinition: false, scopeLine: 1004, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!887 = !DISubprogram(name: "empty", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5emptyEv", scope: !303, file: !12, line: 1008, type: !888, isLocal: false, isDefinition: false, scopeLine: 1008, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!888 = !DISubroutineType(types: !889)
!889 = !{!117, !669}
!890 = !DISubprogram(name: "size", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE4sizeEv", scope: !303, file: !12, line: 1012, type: !891, isLocal: false, isDefinition: false, scopeLine: 1012, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!891 = !DISubroutineType(types: !892)
!892 = !{!893, !669}
!893 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !12, line: 580, baseType: !175)
!894 = !DISubprogram(name: "max_size", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8max_sizeEv", scope: !303, file: !12, line: 1016, type: !891, isLocal: false, isDefinition: false, scopeLine: 1016, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!895 = !DISubprogram(name: "swap", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE4swapERSE_", scope: !303, file: !12, line: 1020, type: !896, isLocal: false, isDefinition: false, scopeLine: 1020, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!896 = !DISubroutineType(types: !897)
!897 = !{null, !663, !862}
!898 = !DISubprogram(name: "_M_erase_aux", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_erase_auxESt23_Rb_tree_const_iteratorIS8_E", scope: !303, file: !12, line: 1113, type: !899, isLocal: false, isDefinition: false, scopeLine: 1113, flags: DIFlagPrototyped, isOptimized: true)
!899 = !DISubroutineType(types: !900)
!900 = !{null, !663, !804}
!901 = !DISubprogram(name: "_M_erase_aux", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_erase_auxESt23_Rb_tree_const_iteratorIS8_ESG_", scope: !303, file: !12, line: 1116, type: !902, isLocal: false, isDefinition: false, scopeLine: 1116, flags: DIFlagPrototyped, isOptimized: true)
!902 = !DISubroutineType(types: !903)
!903 = !{null, !663, !804, !804}
!904 = !DISubprogram(name: "erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5eraseB5cxx11ESt23_Rb_tree_const_iteratorIS8_E", scope: !303, file: !12, line: 1124, type: !905, isLocal: false, isDefinition: false, scopeLine: 1124, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!905 = !DISubroutineType(types: !906)
!906 = !{!810, !663, !804}
!907 = !DISubprogram(name: "erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5eraseB5cxx11ESt17_Rb_tree_iteratorIS8_E", scope: !303, file: !12, line: 1136, type: !908, isLocal: false, isDefinition: false, scopeLine: 1136, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!908 = !DISubroutineType(types: !909)
!909 = !{!810, !663, !810}
!910 = !DISubprogram(name: "erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5eraseERS7_", scope: !303, file: !12, line: 1160, type: !911, isLocal: false, isDefinition: false, scopeLine: 1160, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!911 = !DISubroutineType(types: !912)
!912 = !{!913, !663, !797}
!913 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !303, file: !12, line: 580, baseType: !175)
!914 = !DISubprogram(name: "erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5eraseB5cxx11ESt23_Rb_tree_const_iteratorIS8_ESG_", scope: !303, file: !12, line: 1167, type: !915, isLocal: false, isDefinition: false, scopeLine: 1167, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!915 = !DISubroutineType(types: !916)
!916 = !{!810, !663, !804, !804}
!917 = !DISubprogram(name: "erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5eraseEPS7_SF_", scope: !303, file: !12, line: 1182, type: !918, isLocal: false, isDefinition: false, scopeLine: 1182, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!918 = !DISubroutineType(types: !919)
!919 = !{null, !663, !920, !920}
!920 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !798, size: 64)
!921 = !DISubprogram(name: "clear", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5clearEv", scope: !303, file: !12, line: 1185, type: !832, isLocal: false, isDefinition: false, scopeLine: 1185, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!922 = !DISubprogram(name: "find", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE4findERS7_", scope: !303, file: !12, line: 1193, type: !923, isLocal: false, isDefinition: false, scopeLine: 1193, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!923 = !DISubroutineType(types: !924)
!924 = !{!810, !663, !797}
!925 = !DISubprogram(name: "find", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE4findERS7_", scope: !303, file: !12, line: 1196, type: !926, isLocal: false, isDefinition: false, scopeLine: 1196, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!926 = !DISubroutineType(types: !927)
!927 = !{!804, !669, !797}
!928 = !DISubprogram(name: "count", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE5countERS7_", scope: !303, file: !12, line: 1199, type: !929, isLocal: false, isDefinition: false, scopeLine: 1199, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!929 = !DISubroutineType(types: !930)
!930 = !{!913, !669, !797}
!931 = !DISubprogram(name: "lower_bound", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11lower_boundERS7_", scope: !303, file: !12, line: 1202, type: !923, isLocal: false, isDefinition: false, scopeLine: 1202, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!932 = !DISubprogram(name: "lower_bound", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11lower_boundERS7_", scope: !303, file: !12, line: 1206, type: !926, isLocal: false, isDefinition: false, scopeLine: 1206, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!933 = !DISubprogram(name: "upper_bound", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11upper_boundERS7_", scope: !303, file: !12, line: 1210, type: !923, isLocal: false, isDefinition: false, scopeLine: 1210, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!934 = !DISubprogram(name: "upper_bound", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11upper_boundERS7_", scope: !303, file: !12, line: 1214, type: !926, isLocal: false, isDefinition: false, scopeLine: 1214, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!935 = !DISubprogram(name: "equal_range", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11equal_rangeERS7_", scope: !303, file: !12, line: 1218, type: !936, isLocal: false, isDefinition: false, scopeLine: 1218, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!936 = !DISubroutineType(types: !937)
!937 = !{!938, !663, !797}
!938 = !DICompositeType(tag: DW_TAG_structure_type, name: "pair<std::_Rb_tree_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, std::_Rb_tree_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !72, line: 208, flags: DIFlagFwdDecl, identifier: "_ZTSSt4pairISt17_Rb_tree_iteratorIS_IKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EES9_E")
!939 = !DISubprogram(name: "equal_range", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11equal_rangeERS7_", scope: !303, file: !12, line: 1221, type: !940, isLocal: false, isDefinition: false, scopeLine: 1221, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!940 = !DISubroutineType(types: !941)
!941 = !{!942, !669, !797}
!942 = !DICompositeType(tag: DW_TAG_structure_type, name: "pair<std::_Rb_tree_const_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, std::_Rb_tree_const_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !72, line: 208, flags: DIFlagFwdDecl, identifier: "_ZTSSt4pairISt23_Rb_tree_const_iteratorIS_IKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EES9_E")
!943 = !DISubprogram(name: "__rb_verify", linkageName: "_ZNKSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11__rb_verifyEv", scope: !303, file: !12, line: 1342, type: !888, isLocal: false, isDefinition: false, scopeLine: 1342, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!944 = !DISubprogram(name: "operator=", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EEaSEOSE_", scope: !303, file: !12, line: 1346, type: !945, isLocal: false, isDefinition: false, scopeLine: 1346, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!945 = !DISubroutineType(types: !946)
!946 = !{!862, !663, !851}
!947 = !DISubprogram(name: "_M_move_data", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_move_dataERSE_St17integral_constantIbLb1EE", scope: !303, file: !12, line: 1361, type: !948, isLocal: false, isDefinition: false, scopeLine: 1361, flags: DIFlagPrototyped, isOptimized: true)
!948 = !DISubroutineType(types: !949)
!949 = !{null, !663, !862, !950}
!950 = !DIDerivedType(tag: DW_TAG_typedef, name: "true_type", scope: !13, file: !112, line: 75, baseType: !951)
!951 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "integral_constant<bool, true>", scope: !13, file: !112, line: 57, size: 8, flags: DIFlagTypePassByValue, elements: !952, templateParams: !962, identifier: "_ZTSSt17integral_constantIbLb1EE")
!952 = !{!953, !955, !961}
!953 = !DIDerivedType(tag: DW_TAG_member, name: "value", scope: !951, file: !112, line: 59, baseType: !954, flags: DIFlagStaticMember, extraData: i1 true)
!954 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !117)
!955 = !DISubprogram(name: "operator bool", linkageName: "_ZNKSt17integral_constantIbLb1EEcvbEv", scope: !951, file: !112, line: 62, type: !956, isLocal: false, isDefinition: false, scopeLine: 62, flags: DIFlagPrototyped, isOptimized: true)
!956 = !DISubroutineType(types: !957)
!957 = !{!958, !959}
!958 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !951, file: !112, line: 60, baseType: !117)
!959 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !960, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!960 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !951)
!961 = !DISubprogram(name: "operator()", linkageName: "_ZNKSt17integral_constantIbLb1EEclEv", scope: !951, file: !112, line: 67, type: !956, isLocal: false, isDefinition: false, scopeLine: 67, flags: DIFlagPrototyped, isOptimized: true)
!962 = !{!963, !964}
!963 = !DITemplateTypeParameter(name: "_Tp", type: !117)
!964 = !DITemplateValueParameter(name: "__v", type: !117, value: i8 1)
!965 = !DISubprogram(name: "_M_move_data", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_move_dataERSE_St17integral_constantIbLb0EE", scope: !303, file: !12, line: 1367, type: !966, isLocal: false, isDefinition: false, scopeLine: 1367, flags: DIFlagPrototyped, isOptimized: true)
!966 = !DISubroutineType(types: !967)
!967 = !{null, !663, !862, !968}
!968 = !DIDerivedType(tag: DW_TAG_typedef, name: "false_type", scope: !13, file: !112, line: 78, baseType: !969)
!969 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "integral_constant<bool, false>", scope: !13, file: !112, line: 57, size: 8, flags: DIFlagTypePassByValue, elements: !970, templateParams: !979, identifier: "_ZTSSt17integral_constantIbLb0EE")
!970 = !{!971, !972, !978}
!971 = !DIDerivedType(tag: DW_TAG_member, name: "value", scope: !969, file: !112, line: 59, baseType: !954, flags: DIFlagStaticMember, extraData: i1 false)
!972 = !DISubprogram(name: "operator bool", linkageName: "_ZNKSt17integral_constantIbLb0EEcvbEv", scope: !969, file: !112, line: 62, type: !973, isLocal: false, isDefinition: false, scopeLine: 62, flags: DIFlagPrototyped, isOptimized: true)
!973 = !DISubroutineType(types: !974)
!974 = !{!975, !976}
!975 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !969, file: !112, line: 60, baseType: !117)
!976 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !977, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!977 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !969)
!978 = !DISubprogram(name: "operator()", linkageName: "_ZNKSt17integral_constantIbLb0EEclEv", scope: !969, file: !112, line: 67, type: !973, isLocal: false, isDefinition: false, scopeLine: 67, flags: DIFlagPrototyped, isOptimized: true)
!979 = !{!963, !980}
!980 = !DITemplateValueParameter(name: "__v", type: !117, value: i8 0)
!981 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_move_assignERSE_St17integral_constantIbLb1EE", scope: !303, file: !12, line: 1371, type: !948, isLocal: false, isDefinition: false, scopeLine: 1371, flags: DIFlagPrototyped, isOptimized: true)
!982 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE14_M_move_assignERSE_St17integral_constantIbLb0EE", scope: !303, file: !12, line: 1376, type: !966, isLocal: false, isDefinition: false, scopeLine: 1376, flags: DIFlagPrototyped, isOptimized: true)
!983 = !{!984, !528, !985, !987, !446}
!984 = !DITemplateTypeParameter(name: "_Key", type: !31)
!985 = !DITemplateTypeParameter(name: "_KeyOfValue", type: !986)
!986 = !DICompositeType(tag: DW_TAG_structure_type, name: "_Select1st<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !587, line: 1123, flags: DIFlagFwdDecl, identifier: "_ZTSSt10_Select1stISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE")
!987 = !DITemplateTypeParameter(name: "_Compare", type: !586)
!988 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !989, size: 64)
!989 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Tp_alloc_type", scope: !990, file: !49, line: 84, baseType: !1156)
!990 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Vector_base<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > >", scope: !13, file: !49, line: 81, size: 192, flags: DIFlagTypePassByReference, elements: !991, templateParams: !1155, identifier: "_ZTSSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE")
!991 = !{!992, !1109, !1114, !1119, !1123, !1126, !1131, !1134, !1137, !1140, !1144, !1147, !1148, !1151, !1154}
!992 = !DIDerivedType(tag: DW_TAG_member, name: "_M_impl", scope: !990, file: !49, line: 290, baseType: !993, size: 192)
!993 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Vector_impl", scope: !990, file: !49, line: 88, size: 192, flags: DIFlagTypePassByReference, elements: !994, identifier: "_ZTSNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE12_Vector_implE")
!994 = !{!995, !996, !1090, !1091, !1092, !1096, !1101, !1105}
!995 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !993, baseType: !989, extraData: i32 0)
!996 = !DIDerivedType(tag: DW_TAG_member, name: "_M_start", scope: !993, file: !49, line: 91, baseType: !997, size: 64)
!997 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !990, file: !49, line: 86, baseType: !998)
!998 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !999, file: !59, line: 59, baseType: !1007)
!999 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__alloc_traits<std::allocator<std::__cxx11::basic_string<char> >, std::__cxx11::basic_string<char> >", scope: !5, file: !59, line: 50, size: 8, flags: DIFlagTypePassByValue, elements: !1000, templateParams: !1088, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_EE")
!1000 = !{!1001, !1076, !1079, !1083, !1084, !1085, !1086, !1087}
!1001 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !999, baseType: !1002, extraData: i32 0)
!1002 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "allocator_traits<std::allocator<std::__cxx11::basic_string<char> > >", scope: !13, file: !64, line: 384, size: 8, flags: DIFlagTypePassByValue, elements: !1003, templateParams: !1074, identifier: "_ZTSSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE")
!1003 = !{!1004, !1059, !1062, !1065, !1071}
!1004 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8allocateERS6_m", scope: !1002, file: !64, line: 435, type: !1005, isLocal: false, isDefinition: false, scopeLine: 435, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1005 = !DISubroutineType(types: !1006)
!1006 = !{!1007, !1009, !198}
!1007 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1002, file: !64, line: 392, baseType: !1008)
!1008 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !31, size: 64)
!1009 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1010, size: 64)
!1010 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !1002, file: !64, line: 387, baseType: !1011)
!1011 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "allocator<std::__cxx11::basic_string<char> >", scope: !13, file: !141, line: 108, size: 8, flags: DIFlagTypePassByReference, elements: !1012, templateParams: !601, identifier: "_ZTSSaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE")
!1012 = !{!1013, !1049, !1053, !1058}
!1013 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !1011, baseType: !1014, flags: DIFlagPublic, extraData: i32 0)
!1014 = !DIDerivedType(tag: DW_TAG_typedef, name: "__allocator_base<std::__cxx11::basic_string<char> >", scope: !13, file: !145, line: 48, baseType: !1015)
!1015 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "new_allocator<std::__cxx11::basic_string<char> >", scope: !5, file: !147, line: 58, size: 8, flags: DIFlagTypePassByReference, elements: !1016, templateParams: !601, identifier: "_ZTSN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE")
!1016 = !{!1017, !1021, !1026, !1027, !1034, !1040, !1043, !1046}
!1017 = !DISubprogram(name: "new_allocator", scope: !1015, file: !147, line: 79, type: !1018, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1018 = !DISubroutineType(types: !1019)
!1019 = !{null, !1020}
!1020 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1015, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1021 = !DISubprogram(name: "new_allocator", scope: !1015, file: !147, line: 81, type: !1022, isLocal: false, isDefinition: false, scopeLine: 81, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1022 = !DISubroutineType(types: !1023)
!1023 = !{null, !1020, !1024}
!1024 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1025, size: 64)
!1025 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1015)
!1026 = !DISubprogram(name: "~new_allocator", scope: !1015, file: !147, line: 86, type: !1018, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1027 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE7addressERS6_", scope: !1015, file: !147, line: 89, type: !1028, isLocal: false, isDefinition: false, scopeLine: 89, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1028 = !DISubroutineType(types: !1029)
!1029 = !{!1030, !1031, !1032}
!1030 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1015, file: !147, line: 63, baseType: !1008)
!1031 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1025, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1032 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !1015, file: !147, line: 65, baseType: !1033)
!1033 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !31, size: 64)
!1034 = !DISubprogram(name: "address", linkageName: "_ZNK9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE7addressERKS6_", scope: !1015, file: !147, line: 93, type: !1035, isLocal: false, isDefinition: false, scopeLine: 93, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1035 = !DISubroutineType(types: !1036)
!1036 = !{!1037, !1031, !1039}
!1037 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_pointer", scope: !1015, file: !147, line: 64, baseType: !1038)
!1038 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !343, size: 64)
!1039 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !1015, file: !147, line: 66, baseType: !600)
!1040 = !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE8allocateEmPKv", scope: !1015, file: !147, line: 99, type: !1041, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1041 = !DISubroutineType(types: !1042)
!1042 = !{!1030, !1020, !174, !178}
!1043 = !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE10deallocateEPS6_m", scope: !1015, file: !147, line: 116, type: !1044, isLocal: false, isDefinition: false, scopeLine: 116, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1044 = !DISubroutineType(types: !1045)
!1045 = !{null, !1020, !1030, !174}
!1046 = !DISubprogram(name: "max_size", linkageName: "_ZNK9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE8max_sizeEv", scope: !1015, file: !147, line: 129, type: !1047, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1047 = !DISubroutineType(types: !1048)
!1048 = !{!174, !1031}
!1049 = !DISubprogram(name: "allocator", scope: !1011, file: !141, line: 131, type: !1050, isLocal: false, isDefinition: false, scopeLine: 131, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1050 = !DISubroutineType(types: !1051)
!1051 = !{null, !1052}
!1052 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1011, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1053 = !DISubprogram(name: "allocator", scope: !1011, file: !141, line: 133, type: !1054, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1054 = !DISubroutineType(types: !1055)
!1055 = !{null, !1052, !1056}
!1056 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1057, size: 64)
!1057 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1011)
!1058 = !DISubprogram(name: "~allocator", scope: !1011, file: !141, line: 139, type: !1050, isLocal: false, isDefinition: false, scopeLine: 139, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1059 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8allocateERS6_mPKv", scope: !1002, file: !64, line: 449, type: !1060, isLocal: false, isDefinition: false, scopeLine: 449, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1060 = !DISubroutineType(types: !1061)
!1061 = !{!1007, !1009, !198, !202}
!1062 = !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE10deallocateERS6_PS5_m", scope: !1002, file: !64, line: 461, type: !1063, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1063 = !DISubroutineType(types: !1064)
!1064 = !{null, !1009, !1007, !198}
!1065 = !DISubprogram(name: "max_size", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8max_sizeERKS6_", scope: !1002, file: !64, line: 495, type: !1066, isLocal: false, isDefinition: false, scopeLine: 495, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1066 = !DISubroutineType(types: !1067)
!1067 = !{!1068, !1069}
!1068 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !1002, file: !64, line: 407, baseType: !175)
!1069 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1070, size: 64)
!1070 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1010)
!1071 = !DISubprogram(name: "select_on_container_copy_construction", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE37select_on_container_copy_constructionERKS6_", scope: !1002, file: !64, line: 504, type: !1072, isLocal: false, isDefinition: false, scopeLine: 504, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1072 = !DISubroutineType(types: !1073)
!1073 = !{!1010, !1069}
!1074 = !{!1075}
!1075 = !DITemplateTypeParameter(name: "_Alloc", type: !1011)
!1076 = !DISubprogram(name: "_S_select_on_copy", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E17_S_select_on_copyERKS7_", scope: !999, file: !59, line: 94, type: !1077, isLocal: false, isDefinition: false, scopeLine: 94, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1077 = !DISubroutineType(types: !1078)
!1078 = !{!1011, !1056}
!1079 = !DISubprogram(name: "_S_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E10_S_on_swapERS7_S9_", scope: !999, file: !59, line: 97, type: !1080, isLocal: false, isDefinition: false, scopeLine: 97, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1080 = !DISubroutineType(types: !1081)
!1081 = !{null, !1082, !1082}
!1082 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1011, size: 64)
!1083 = !DISubprogram(name: "_S_propagate_on_copy_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E27_S_propagate_on_copy_assignEv", scope: !999, file: !59, line: 100, type: !225, isLocal: false, isDefinition: false, scopeLine: 100, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1084 = !DISubprogram(name: "_S_propagate_on_move_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E27_S_propagate_on_move_assignEv", scope: !999, file: !59, line: 103, type: !225, isLocal: false, isDefinition: false, scopeLine: 103, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1085 = !DISubprogram(name: "_S_propagate_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E20_S_propagate_on_swapEv", scope: !999, file: !59, line: 106, type: !225, isLocal: false, isDefinition: false, scopeLine: 106, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1086 = !DISubprogram(name: "_S_always_equal", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E15_S_always_equalEv", scope: !999, file: !59, line: 109, type: !225, isLocal: false, isDefinition: false, scopeLine: 109, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1087 = !DISubprogram(name: "_S_nothrow_move", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E15_S_nothrow_moveEv", scope: !999, file: !59, line: 112, type: !225, isLocal: false, isDefinition: false, scopeLine: 112, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1088 = !{!1075, !1089}
!1089 = !DITemplateTypeParameter(type: !31)
!1090 = !DIDerivedType(tag: DW_TAG_member, name: "_M_finish", scope: !993, file: !49, line: 92, baseType: !997, size: 64, offset: 64)
!1091 = !DIDerivedType(tag: DW_TAG_member, name: "_M_end_of_storage", scope: !993, file: !49, line: 93, baseType: !997, size: 64, offset: 128)
!1092 = !DISubprogram(name: "_Vector_impl", scope: !993, file: !49, line: 95, type: !1093, isLocal: false, isDefinition: false, scopeLine: 95, flags: DIFlagPrototyped, isOptimized: true)
!1093 = !DISubroutineType(types: !1094)
!1094 = !{null, !1095}
!1095 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !993, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1096 = !DISubprogram(name: "_Vector_impl", scope: !993, file: !49, line: 99, type: !1097, isLocal: false, isDefinition: false, scopeLine: 99, flags: DIFlagPrototyped, isOptimized: true)
!1097 = !DISubroutineType(types: !1098)
!1098 = !{null, !1095, !1099}
!1099 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1100, size: 64)
!1100 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !989)
!1101 = !DISubprogram(name: "_Vector_impl", scope: !993, file: !49, line: 104, type: !1102, isLocal: false, isDefinition: false, scopeLine: 104, flags: DIFlagPrototyped, isOptimized: true)
!1102 = !DISubroutineType(types: !1103)
!1103 = !{null, !1095, !1104}
!1104 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !989, size: 64)
!1105 = !DISubprogram(name: "_M_swap_data", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE12_Vector_impl12_M_swap_dataERS8_", scope: !993, file: !49, line: 110, type: !1106, isLocal: false, isDefinition: false, scopeLine: 110, flags: DIFlagPrototyped, isOptimized: true)
!1106 = !DISubroutineType(types: !1107)
!1107 = !{null, !1095, !1108}
!1108 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !993, size: 64)
!1109 = !DISubprogram(name: "_M_get_Tp_allocator", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_get_Tp_allocatorEv", scope: !990, file: !49, line: 237, type: !1110, isLocal: false, isDefinition: false, scopeLine: 237, flags: DIFlagPrototyped, isOptimized: true)
!1110 = !DISubroutineType(types: !1111)
!1111 = !{!1112, !1113}
!1112 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !989, size: 64)
!1113 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !990, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1114 = !DISubprogram(name: "_M_get_Tp_allocator", linkageName: "_ZNKSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_get_Tp_allocatorEv", scope: !990, file: !49, line: 241, type: !1115, isLocal: false, isDefinition: false, scopeLine: 241, flags: DIFlagPrototyped, isOptimized: true)
!1115 = !DISubroutineType(types: !1116)
!1116 = !{!1099, !1117}
!1117 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1118, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1118 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !990)
!1119 = !DISubprogram(name: "get_allocator", linkageName: "_ZNKSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE13get_allocatorEv", scope: !990, file: !49, line: 245, type: !1120, isLocal: false, isDefinition: false, scopeLine: 245, flags: DIFlagPrototyped, isOptimized: true)
!1120 = !DISubroutineType(types: !1121)
!1121 = !{!1122, !1117}
!1122 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !990, file: !49, line: 234, baseType: !1011)
!1123 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 248, type: !1124, isLocal: false, isDefinition: false, scopeLine: 248, flags: DIFlagPrototyped, isOptimized: true)
!1124 = !DISubroutineType(types: !1125)
!1125 = !{null, !1113}
!1126 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 251, type: !1127, isLocal: false, isDefinition: false, scopeLine: 251, flags: DIFlagPrototyped, isOptimized: true)
!1127 = !DISubroutineType(types: !1128)
!1128 = !{null, !1113, !1129}
!1129 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1130, size: 64)
!1130 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1122)
!1131 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 254, type: !1132, isLocal: false, isDefinition: false, scopeLine: 254, flags: DIFlagPrototyped, isOptimized: true)
!1132 = !DISubroutineType(types: !1133)
!1133 = !{null, !1113, !175}
!1134 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 258, type: !1135, isLocal: false, isDefinition: false, scopeLine: 258, flags: DIFlagPrototyped, isOptimized: true)
!1135 = !DISubroutineType(types: !1136)
!1136 = !{null, !1113, !175, !1129}
!1137 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 263, type: !1138, isLocal: false, isDefinition: false, scopeLine: 263, flags: DIFlagPrototyped, isOptimized: true)
!1138 = !DISubroutineType(types: !1139)
!1139 = !{null, !1113, !1104}
!1140 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 266, type: !1141, isLocal: false, isDefinition: false, scopeLine: 266, flags: DIFlagPrototyped, isOptimized: true)
!1141 = !DISubroutineType(types: !1142)
!1142 = !{null, !1113, !1143}
!1143 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !990, size: 64)
!1144 = !DISubprogram(name: "_Vector_base", scope: !990, file: !49, line: 270, type: !1145, isLocal: false, isDefinition: false, scopeLine: 270, flags: DIFlagPrototyped, isOptimized: true)
!1145 = !DISubroutineType(types: !1146)
!1146 = !{null, !1113, !1143, !1129}
!1147 = !DISubprogram(name: "~_Vector_base", scope: !990, file: !49, line: 283, type: !1124, isLocal: false, isDefinition: false, scopeLine: 283, flags: DIFlagPrototyped, isOptimized: true)
!1148 = !DISubprogram(name: "_M_allocate", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE11_M_allocateEm", scope: !990, file: !49, line: 293, type: !1149, isLocal: false, isDefinition: false, scopeLine: 293, flags: DIFlagPrototyped, isOptimized: true)
!1149 = !DISubroutineType(types: !1150)
!1150 = !{!997, !1113, !175}
!1151 = !DISubprogram(name: "_M_deallocate", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE13_M_deallocateEPS5_m", scope: !990, file: !49, line: 300, type: !1152, isLocal: false, isDefinition: false, scopeLine: 300, flags: DIFlagPrototyped, isOptimized: true)
!1152 = !DISubroutineType(types: !1153)
!1153 = !{null, !1113, !997, !175}
!1154 = !DISubprogram(name: "_M_create_storage", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE17_M_create_storageEm", scope: !990, file: !49, line: 309, type: !1132, isLocal: false, isDefinition: false, scopeLine: 309, flags: DIFlagPrivate | DIFlagPrototyped, isOptimized: true)
!1155 = !{!602, !1075}
!1156 = !DIDerivedType(tag: DW_TAG_typedef, name: "other", scope: !1157, file: !59, line: 117, baseType: !1158)
!1157 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "rebind<std::__cxx11::basic_string<char> >", scope: !999, file: !59, line: 116, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !601, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEES6_E6rebindIS6_EE")
!1158 = !DIDerivedType(tag: DW_TAG_typedef, name: "rebind_alloc<std::__cxx11::basic_string<char> >", scope: !1002, file: !64, line: 422, baseType: !1011)
!1159 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !31, file: !30, line: 88, baseType: !1160)
!1160 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !1161, file: !59, line: 61, baseType: !1184)
!1161 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__alloc_traits<std::allocator<char>, char>", scope: !5, file: !59, line: 50, size: 8, flags: DIFlagTypePassByValue, elements: !1162, templateParams: !1206, identifier: "_ZTSN9__gnu_cxx14__alloc_traitsISaIcEcEE")
!1162 = !{!1163, !1192, !1197, !1201, !1202, !1203, !1204, !1205}
!1163 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !1161, baseType: !1164, extraData: i32 0)
!1164 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "allocator_traits<std::allocator<char> >", scope: !13, file: !64, line: 384, size: 8, flags: DIFlagTypePassByValue, elements: !1165, templateParams: !1190, identifier: "_ZTSSt16allocator_traitsISaIcEE")
!1165 = !{!1166, !1175, !1178, !1181, !1187}
!1166 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaIcEE8allocateERS0_m", scope: !1164, file: !64, line: 435, type: !1167, isLocal: false, isDefinition: false, scopeLine: 435, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1167 = !DISubroutineType(types: !1168)
!1168 = !{!1169, !1172, !198}
!1169 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1164, file: !64, line: 392, baseType: !1170)
!1170 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1171, size: 64)
!1171 = !DIBasicType(name: "char", size: 8, encoding: DW_ATE_signed_char)
!1172 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1173, size: 64)
!1173 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !1164, file: !64, line: 387, baseType: !1174)
!1174 = !DICompositeType(tag: DW_TAG_class_type, name: "allocator<char>", scope: !13, file: !141, line: 199, flags: DIFlagFwdDecl, identifier: "_ZTSSaIcE")
!1175 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaIcEE8allocateERS0_mPKv", scope: !1164, file: !64, line: 449, type: !1176, isLocal: false, isDefinition: false, scopeLine: 449, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1176 = !DISubroutineType(types: !1177)
!1177 = !{!1169, !1172, !198, !202}
!1178 = !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaIcEE10deallocateERS0_Pcm", scope: !1164, file: !64, line: 461, type: !1179, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1179 = !DISubroutineType(types: !1180)
!1180 = !{null, !1172, !1169, !198}
!1181 = !DISubprogram(name: "max_size", linkageName: "_ZNSt16allocator_traitsISaIcEE8max_sizeERKS0_", scope: !1164, file: !64, line: 495, type: !1182, isLocal: false, isDefinition: false, scopeLine: 495, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1182 = !DISubroutineType(types: !1183)
!1183 = !{!1184, !1185}
!1184 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !1164, file: !64, line: 407, baseType: !175)
!1185 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1186, size: 64)
!1186 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1173)
!1187 = !DISubprogram(name: "select_on_container_copy_construction", linkageName: "_ZNSt16allocator_traitsISaIcEE37select_on_container_copy_constructionERKS0_", scope: !1164, file: !64, line: 504, type: !1188, isLocal: false, isDefinition: false, scopeLine: 504, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1188 = !DISubroutineType(types: !1189)
!1189 = !{!1173, !1185}
!1190 = !{!1191}
!1191 = !DITemplateTypeParameter(name: "_Alloc", type: !1174)
!1192 = !DISubprogram(name: "_S_select_on_copy", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE17_S_select_on_copyERKS1_", scope: !1161, file: !59, line: 94, type: !1193, isLocal: false, isDefinition: false, scopeLine: 94, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1193 = !DISubroutineType(types: !1194)
!1194 = !{!1174, !1195}
!1195 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1196, size: 64)
!1196 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1174)
!1197 = !DISubprogram(name: "_S_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE10_S_on_swapERS1_S3_", scope: !1161, file: !59, line: 97, type: !1198, isLocal: false, isDefinition: false, scopeLine: 97, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1198 = !DISubroutineType(types: !1199)
!1199 = !{null, !1200, !1200}
!1200 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1174, size: 64)
!1201 = !DISubprogram(name: "_S_propagate_on_copy_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE27_S_propagate_on_copy_assignEv", scope: !1161, file: !59, line: 100, type: !225, isLocal: false, isDefinition: false, scopeLine: 100, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1202 = !DISubprogram(name: "_S_propagate_on_move_assign", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE27_S_propagate_on_move_assignEv", scope: !1161, file: !59, line: 103, type: !225, isLocal: false, isDefinition: false, scopeLine: 103, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1203 = !DISubprogram(name: "_S_propagate_on_swap", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE20_S_propagate_on_swapEv", scope: !1161, file: !59, line: 106, type: !225, isLocal: false, isDefinition: false, scopeLine: 106, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1204 = !DISubprogram(name: "_S_always_equal", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE15_S_always_equalEv", scope: !1161, file: !59, line: 109, type: !225, isLocal: false, isDefinition: false, scopeLine: 109, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1205 = !DISubprogram(name: "_S_nothrow_move", linkageName: "_ZN9__gnu_cxx14__alloc_traitsISaIcEcE15_S_nothrow_moveEv", scope: !1161, file: !59, line: 112, type: !225, isLocal: false, isDefinition: false, scopeLine: 112, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1206 = !{!1191, !1207}
!1207 = !DITemplateTypeParameter(type: !1171)
!1208 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1209, size: 64)
!1209 = !DIDerivedType(tag: DW_TAG_typedef, name: "char_type", scope: !1211, file: !1210, line: 277, baseType: !1171)
!1210 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/char_traits.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1211 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "char_traits<char>", scope: !13, file: !1210, line: 275, size: 8, flags: DIFlagTypePassByValue, elements: !1212, templateParams: !1258, identifier: "_ZTSSt11char_traitsIcE")
!1212 = !{!1213, !1219, !1222, !1223, !1227, !1230, !1233, !1236, !1237, !1240, !1246, !1249, !1252, !1255}
!1213 = !DISubprogram(name: "assign", linkageName: "_ZNSt11char_traitsIcE6assignERcRKc", scope: !1211, file: !1210, line: 284, type: !1214, isLocal: false, isDefinition: false, scopeLine: 284, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1214 = !DISubroutineType(types: !1215)
!1215 = !{null, !1216, !1217}
!1216 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1209, size: 64)
!1217 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1218, size: 64)
!1218 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1209)
!1219 = !DISubprogram(name: "eq", linkageName: "_ZNSt11char_traitsIcE2eqERKcS2_", scope: !1211, file: !1210, line: 288, type: !1220, isLocal: false, isDefinition: false, scopeLine: 288, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1220 = !DISubroutineType(types: !1221)
!1221 = !{!117, !1217, !1217}
!1222 = !DISubprogram(name: "lt", linkageName: "_ZNSt11char_traitsIcE2ltERKcS2_", scope: !1211, file: !1210, line: 292, type: !1220, isLocal: false, isDefinition: false, scopeLine: 292, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1223 = !DISubprogram(name: "compare", linkageName: "_ZNSt11char_traitsIcE7compareEPKcS2_m", scope: !1211, file: !1210, line: 300, type: !1224, isLocal: false, isDefinition: false, scopeLine: 300, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1224 = !DISubroutineType(types: !1225)
!1225 = !{!93, !1226, !1226, !175}
!1226 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1218, size: 64)
!1227 = !DISubprogram(name: "length", linkageName: "_ZNSt11char_traitsIcE6lengthEPKc", scope: !1211, file: !1210, line: 314, type: !1228, isLocal: false, isDefinition: false, scopeLine: 314, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1228 = !DISubroutineType(types: !1229)
!1229 = !{!175, !1226}
!1230 = !DISubprogram(name: "find", linkageName: "_ZNSt11char_traitsIcE4findEPKcmRS1_", scope: !1211, file: !1210, line: 324, type: !1231, isLocal: false, isDefinition: false, scopeLine: 324, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1231 = !DISubroutineType(types: !1232)
!1232 = !{!1226, !1226, !175, !1217}
!1233 = !DISubprogram(name: "move", linkageName: "_ZNSt11char_traitsIcE4moveEPcPKcm", scope: !1211, file: !1210, line: 338, type: !1234, isLocal: false, isDefinition: false, scopeLine: 338, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1234 = !DISubroutineType(types: !1235)
!1235 = !{!1208, !1208, !1226, !175}
!1236 = !DISubprogram(name: "copy", linkageName: "_ZNSt11char_traitsIcE4copyEPcPKcm", scope: !1211, file: !1210, line: 346, type: !1234, isLocal: false, isDefinition: false, scopeLine: 346, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1237 = !DISubprogram(name: "assign", linkageName: "_ZNSt11char_traitsIcE6assignEPcmc", scope: !1211, file: !1210, line: 354, type: !1238, isLocal: false, isDefinition: false, scopeLine: 354, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1238 = !DISubroutineType(types: !1239)
!1239 = !{!1208, !1208, !175, !1209}
!1240 = !DISubprogram(name: "to_char_type", linkageName: "_ZNSt11char_traitsIcE12to_char_typeERKi", scope: !1211, file: !1210, line: 362, type: !1241, isLocal: false, isDefinition: false, scopeLine: 362, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1241 = !DISubroutineType(types: !1242)
!1242 = !{!1209, !1243}
!1243 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1244, size: 64)
!1244 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1245)
!1245 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_type", scope: !1211, file: !1210, line: 278, baseType: !93)
!1246 = !DISubprogram(name: "to_int_type", linkageName: "_ZNSt11char_traitsIcE11to_int_typeERKc", scope: !1211, file: !1210, line: 368, type: !1247, isLocal: false, isDefinition: false, scopeLine: 368, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1247 = !DISubroutineType(types: !1248)
!1248 = !{!1245, !1217}
!1249 = !DISubprogram(name: "eq_int_type", linkageName: "_ZNSt11char_traitsIcE11eq_int_typeERKiS2_", scope: !1211, file: !1210, line: 372, type: !1250, isLocal: false, isDefinition: false, scopeLine: 372, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1250 = !DISubroutineType(types: !1251)
!1251 = !{!117, !1243, !1243}
!1252 = !DISubprogram(name: "eof", linkageName: "_ZNSt11char_traitsIcE3eofEv", scope: !1211, file: !1210, line: 376, type: !1253, isLocal: false, isDefinition: false, scopeLine: 376, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1253 = !DISubroutineType(types: !1254)
!1254 = !{!1245}
!1255 = !DISubprogram(name: "not_eof", linkageName: "_ZNSt11char_traitsIcE7not_eofERKi", scope: !1211, file: !1210, line: 380, type: !1256, isLocal: false, isDefinition: false, scopeLine: 380, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!1256 = !DISubroutineType(types: !1257)
!1257 = !{!1245, !1243}
!1258 = !{!1259}
!1259 = !DITemplateTypeParameter(name: "_CharT", type: !1171)
!1260 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !49, line: 374, baseType: !175)
!1261 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !243, size: 64)
!1262 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator", scope: !1263, file: !49, line: 369, baseType: !1461)
!1263 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "vector<std::pair<int, int>, std::allocator<std::pair<int, int> > >", scope: !13, file: !49, line: 339, size: 192, flags: DIFlagTypePassByReference, elements: !1264, templateParams: !298, identifier: "_ZTSSt6vectorISt4pairIiiESaIS1_EE")
!1264 = !{!1265, !1266, !1270, !1276, !1279, !1285, !1290, !1294, !1297, !1300, !1305, !1306, !1310, !1313, !1316, !1319, !1322, !1325, !1331, !1332, !1333, !1338, !1343, !1344, !1345, !1346, !1347, !1348, !1349, !1352, !1353, !1356, !1357, !1358, !1359, !1362, !1363, !1371, !1378, !1381, !1382, !1383, !1386, !1389, !1390, !1391, !1394, !1397, !1400, !1404, !1405, !1408, !1411, !1414, !1417, !1420, !1423, !1426, !1427, !1428, !1429, !1430, !1433, !1434, !1437, !1438, !1439, !1445, !1449, !1452, !1455, !1458}
!1265 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !1263, baseType: !50, flags: DIFlagProtected, extraData: i32 0)
!1266 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 391, type: !1267, isLocal: false, isDefinition: false, scopeLine: 391, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1267 = !DISubroutineType(types: !1268)
!1268 = !{null, !1269}
!1269 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1263, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1270 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 402, type: !1271, isLocal: false, isDefinition: false, scopeLine: 402, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!1271 = !DISubroutineType(types: !1272)
!1272 = !{null, !1269, !1273}
!1273 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1274, size: 64)
!1274 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1275)
!1275 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !1263, file: !49, line: 376, baseType: !140)
!1276 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 415, type: !1277, isLocal: false, isDefinition: false, scopeLine: 415, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!1277 = !DISubroutineType(types: !1278)
!1278 = !{null, !1269, !1260, !1273}
!1279 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 427, type: !1280, isLocal: false, isDefinition: false, scopeLine: 427, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1280 = !DISubroutineType(types: !1281)
!1281 = !{null, !1269, !1260, !1282, !1273}
!1282 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1283, size: 64)
!1283 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1284)
!1284 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !1263, file: !49, line: 364, baseType: !71)
!1285 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 458, type: !1286, isLocal: false, isDefinition: false, scopeLine: 458, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1286 = !DISubroutineType(types: !1287)
!1287 = !{null, !1269, !1288}
!1288 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1289, size: 64)
!1289 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1263)
!1290 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 476, type: !1291, isLocal: false, isDefinition: false, scopeLine: 476, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1291 = !DISubroutineType(types: !1292)
!1292 = !{null, !1269, !1293}
!1293 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !1263, size: 64)
!1294 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 480, type: !1295, isLocal: false, isDefinition: false, scopeLine: 480, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1295 = !DISubroutineType(types: !1296)
!1296 = !{null, !1269, !1288, !1273}
!1297 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 490, type: !1298, isLocal: false, isDefinition: false, scopeLine: 490, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1298 = !DISubroutineType(types: !1299)
!1299 = !{null, !1269, !1293, !1273}
!1300 = !DISubprogram(name: "vector", scope: !1263, file: !49, line: 515, type: !1301, isLocal: false, isDefinition: false, scopeLine: 515, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1301 = !DISubroutineType(types: !1302)
!1302 = !{null, !1269, !1303, !1273}
!1303 = !DICompositeType(tag: DW_TAG_class_type, name: "initializer_list<std::pair<int, int> >", scope: !13, file: !1304, line: 47, flags: DIFlagFwdDecl, identifier: "_ZTSSt16initializer_listISt4pairIiiEE")
!1304 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/initializer_list", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1305 = !DISubprogram(name: "~vector", scope: !1263, file: !49, line: 565, type: !1267, isLocal: false, isDefinition: false, scopeLine: 565, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1306 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EEaSERKS3_", scope: !1263, file: !49, line: 582, type: !1307, isLocal: false, isDefinition: false, scopeLine: 582, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1307 = !DISubroutineType(types: !1308)
!1308 = !{!1309, !1269, !1288}
!1309 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1263, size: 64)
!1310 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EEaSEOS3_", scope: !1263, file: !49, line: 596, type: !1311, isLocal: false, isDefinition: false, scopeLine: 596, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1311 = !DISubroutineType(types: !1312)
!1312 = !{!1309, !1269, !1293}
!1313 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EEaSESt16initializer_listIS1_E", scope: !1263, file: !49, line: 617, type: !1314, isLocal: false, isDefinition: false, scopeLine: 617, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1314 = !DISubroutineType(types: !1315)
!1315 = !{!1309, !1269, !1303}
!1316 = !DISubprogram(name: "assign", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6assignEmRKS1_", scope: !1263, file: !49, line: 636, type: !1317, isLocal: false, isDefinition: false, scopeLine: 636, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1317 = !DISubroutineType(types: !1318)
!1318 = !{null, !1269, !1260, !1282}
!1319 = !DISubprogram(name: "assign", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6assignESt16initializer_listIS1_E", scope: !1263, file: !49, line: 681, type: !1320, isLocal: false, isDefinition: false, scopeLine: 681, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1320 = !DISubroutineType(types: !1321)
!1321 = !{null, !1269, !1303}
!1322 = !DISubprogram(name: "begin", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE5beginEv", scope: !1263, file: !49, line: 698, type: !1323, isLocal: false, isDefinition: false, scopeLine: 698, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1323 = !DISubroutineType(types: !1324)
!1324 = !{!1262, !1269}
!1325 = !DISubprogram(name: "begin", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE5beginEv", scope: !1263, file: !49, line: 707, type: !1326, isLocal: false, isDefinition: false, scopeLine: 707, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1326 = !DISubroutineType(types: !1327)
!1327 = !{!1328, !1330}
!1328 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_iterator", scope: !1263, file: !49, line: 371, baseType: !1329)
!1329 = !DICompositeType(tag: DW_TAG_class_type, name: "__normal_iterator<const std::pair<int, int> *, std::vector<std::pair<int, int>, std::allocator<std::pair<int, int> > > >", scope: !5, file: !879, line: 764, flags: DIFlagFwdDecl, identifier: "_ZTSN9__gnu_cxx17__normal_iteratorIPKSt4pairIiiESt6vectorIS2_SaIS2_EEEE")
!1330 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1289, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1331 = !DISubprogram(name: "end", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE3endEv", scope: !1263, file: !49, line: 716, type: !1323, isLocal: false, isDefinition: false, scopeLine: 716, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1332 = !DISubprogram(name: "end", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE3endEv", scope: !1263, file: !49, line: 725, type: !1326, isLocal: false, isDefinition: false, scopeLine: 725, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1333 = !DISubprogram(name: "rbegin", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6rbeginEv", scope: !1263, file: !49, line: 734, type: !1334, isLocal: false, isDefinition: false, scopeLine: 734, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1334 = !DISubroutineType(types: !1335)
!1335 = !{!1336, !1269}
!1336 = !DIDerivedType(tag: DW_TAG_typedef, name: "reverse_iterator", scope: !1263, file: !49, line: 373, baseType: !1337)
!1337 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<__gnu_cxx::__normal_iterator<std::pair<int, int> *, std::vector<std::pair<int, int>, std::allocator<std::pair<int, int> > > > >", scope: !13, file: !879, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorIN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS3_SaIS3_EEEEE")
!1338 = !DISubprogram(name: "rbegin", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE6rbeginEv", scope: !1263, file: !49, line: 743, type: !1339, isLocal: false, isDefinition: false, scopeLine: 743, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1339 = !DISubroutineType(types: !1340)
!1340 = !{!1341, !1330}
!1341 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reverse_iterator", scope: !1263, file: !49, line: 372, baseType: !1342)
!1342 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<__gnu_cxx::__normal_iterator<const std::pair<int, int> *, std::vector<std::pair<int, int>, std::allocator<std::pair<int, int> > > > >", scope: !13, file: !879, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorIN9__gnu_cxx17__normal_iteratorIPKSt4pairIiiESt6vectorIS3_SaIS3_EEEEE")
!1343 = !DISubprogram(name: "rend", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE4rendEv", scope: !1263, file: !49, line: 752, type: !1334, isLocal: false, isDefinition: false, scopeLine: 752, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1344 = !DISubprogram(name: "rend", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE4rendEv", scope: !1263, file: !49, line: 761, type: !1339, isLocal: false, isDefinition: false, scopeLine: 761, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1345 = !DISubprogram(name: "cbegin", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE6cbeginEv", scope: !1263, file: !49, line: 771, type: !1326, isLocal: false, isDefinition: false, scopeLine: 771, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1346 = !DISubprogram(name: "cend", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE4cendEv", scope: !1263, file: !49, line: 780, type: !1326, isLocal: false, isDefinition: false, scopeLine: 780, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1347 = !DISubprogram(name: "crbegin", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE7crbeginEv", scope: !1263, file: !49, line: 789, type: !1339, isLocal: false, isDefinition: false, scopeLine: 789, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1348 = !DISubprogram(name: "crend", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE5crendEv", scope: !1263, file: !49, line: 798, type: !1339, isLocal: false, isDefinition: false, scopeLine: 798, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1349 = !DISubprogram(name: "size", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE4sizeEv", scope: !1263, file: !49, line: 805, type: !1350, isLocal: false, isDefinition: false, scopeLine: 805, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1350 = !DISubroutineType(types: !1351)
!1351 = !{!1260, !1330}
!1352 = !DISubprogram(name: "max_size", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE8max_sizeEv", scope: !1263, file: !49, line: 810, type: !1350, isLocal: false, isDefinition: false, scopeLine: 810, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1353 = !DISubprogram(name: "resize", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6resizeEm", scope: !1263, file: !49, line: 824, type: !1354, isLocal: false, isDefinition: false, scopeLine: 824, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1354 = !DISubroutineType(types: !1355)
!1355 = !{null, !1269, !1260}
!1356 = !DISubprogram(name: "resize", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6resizeEmRKS1_", scope: !1263, file: !49, line: 844, type: !1317, isLocal: false, isDefinition: false, scopeLine: 844, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1357 = !DISubprogram(name: "shrink_to_fit", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE13shrink_to_fitEv", scope: !1263, file: !49, line: 876, type: !1267, isLocal: false, isDefinition: false, scopeLine: 876, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1358 = !DISubprogram(name: "capacity", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE8capacityEv", scope: !1263, file: !49, line: 885, type: !1350, isLocal: false, isDefinition: false, scopeLine: 885, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1359 = !DISubprogram(name: "empty", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE5emptyEv", scope: !1263, file: !49, line: 894, type: !1360, isLocal: false, isDefinition: false, scopeLine: 894, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1360 = !DISubroutineType(types: !1361)
!1361 = !{!117, !1330}
!1362 = !DISubprogram(name: "reserve", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE7reserveEm", scope: !1263, file: !49, line: 915, type: !1354, isLocal: false, isDefinition: false, scopeLine: 915, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1363 = !DISubprogram(name: "operator[]", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EEixEm", scope: !1263, file: !49, line: 930, type: !1364, isLocal: false, isDefinition: false, scopeLine: 930, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1364 = !DISubroutineType(types: !1365)
!1365 = !{!1366, !1269, !1260}
!1366 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !1263, file: !49, line: 367, baseType: !1367)
!1367 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !60, file: !59, line: 64, baseType: !1368)
!1368 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1369, size: 64)
!1369 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !60, file: !59, line: 58, baseType: !1370)
!1370 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !63, file: !64, line: 389, baseType: !71)
!1371 = !DISubprogram(name: "operator[]", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EEixEm", scope: !1263, file: !49, line: 948, type: !1372, isLocal: false, isDefinition: false, scopeLine: 948, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1372 = !DISubroutineType(types: !1373)
!1373 = !{!1374, !1330, !1260}
!1374 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !1263, file: !49, line: 368, baseType: !1375)
!1375 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !60, file: !59, line: 65, baseType: !1376)
!1376 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1377, size: 64)
!1377 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1369)
!1378 = !DISubprogram(name: "_M_range_check", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE14_M_range_checkEm", scope: !1263, file: !49, line: 957, type: !1379, isLocal: false, isDefinition: false, scopeLine: 957, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1379 = !DISubroutineType(types: !1380)
!1380 = !{null, !1330, !1260}
!1381 = !DISubprogram(name: "at", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE2atEm", scope: !1263, file: !49, line: 979, type: !1364, isLocal: false, isDefinition: false, scopeLine: 979, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1382 = !DISubprogram(name: "at", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE2atEm", scope: !1263, file: !49, line: 997, type: !1372, isLocal: false, isDefinition: false, scopeLine: 997, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1383 = !DISubprogram(name: "front", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE5frontEv", scope: !1263, file: !49, line: 1008, type: !1384, isLocal: false, isDefinition: false, scopeLine: 1008, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1384 = !DISubroutineType(types: !1385)
!1385 = !{!1366, !1269}
!1386 = !DISubprogram(name: "front", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE5frontEv", scope: !1263, file: !49, line: 1019, type: !1387, isLocal: false, isDefinition: false, scopeLine: 1019, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1387 = !DISubroutineType(types: !1388)
!1388 = !{!1374, !1330}
!1389 = !DISubprogram(name: "back", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE4backEv", scope: !1263, file: !49, line: 1030, type: !1384, isLocal: false, isDefinition: false, scopeLine: 1030, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1390 = !DISubprogram(name: "back", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE4backEv", scope: !1263, file: !49, line: 1041, type: !1387, isLocal: false, isDefinition: false, scopeLine: 1041, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1391 = !DISubprogram(name: "data", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE4dataEv", scope: !1263, file: !49, line: 1055, type: !1392, isLocal: false, isDefinition: false, scopeLine: 1055, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1392 = !DISubroutineType(types: !1393)
!1393 = !{!70, !1269}
!1394 = !DISubprogram(name: "data", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE4dataEv", scope: !1263, file: !49, line: 1059, type: !1395, isLocal: false, isDefinition: false, scopeLine: 1059, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1395 = !DISubroutineType(types: !1396)
!1396 = !{!169, !1330}
!1397 = !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE9push_backERKS1_", scope: !1263, file: !49, line: 1074, type: !1398, isLocal: false, isDefinition: false, scopeLine: 1074, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1398 = !DISubroutineType(types: !1399)
!1399 = !{null, !1269, !1282}
!1400 = !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE9push_backEOS1_", scope: !1263, file: !49, line: 1090, type: !1401, isLocal: false, isDefinition: false, scopeLine: 1090, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1401 = !DISubroutineType(types: !1402)
!1402 = !{null, !1269, !1403}
!1403 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !1284, size: 64)
!1404 = !DISubprogram(name: "pop_back", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE8pop_backEv", scope: !1263, file: !49, line: 1112, type: !1267, isLocal: false, isDefinition: false, scopeLine: 1112, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1405 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EERS6_", scope: !1263, file: !49, line: 1150, type: !1406, isLocal: false, isDefinition: false, scopeLine: 1150, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1406 = !DISubroutineType(types: !1407)
!1407 = !{!1262, !1269, !1328, !1282}
!1408 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EEOS1_", scope: !1263, file: !49, line: 1180, type: !1409, isLocal: false, isDefinition: false, scopeLine: 1180, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1409 = !DISubroutineType(types: !1410)
!1410 = !{!1262, !1269, !1328, !1403}
!1411 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EESt16initializer_listIS1_E", scope: !1263, file: !49, line: 1197, type: !1412, isLocal: false, isDefinition: false, scopeLine: 1197, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1412 = !DISubroutineType(types: !1413)
!1413 = !{!1262, !1269, !1328, !1303}
!1414 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EEmRS6_", scope: !1263, file: !49, line: 1222, type: !1415, isLocal: false, isDefinition: false, scopeLine: 1222, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1415 = !DISubroutineType(types: !1416)
!1416 = !{!1262, !1269, !1328, !1260, !1282}
!1417 = !DISubprogram(name: "erase", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE5eraseEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EE", scope: !1263, file: !49, line: 1317, type: !1418, isLocal: false, isDefinition: false, scopeLine: 1317, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1418 = !DISubroutineType(types: !1419)
!1419 = !{!1262, !1269, !1328}
!1420 = !DISubprogram(name: "erase", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE5eraseEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EES8_", scope: !1263, file: !49, line: 1344, type: !1421, isLocal: false, isDefinition: false, scopeLine: 1344, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1421 = !DISubroutineType(types: !1422)
!1422 = !{!1262, !1269, !1328, !1328}
!1423 = !DISubprogram(name: "swap", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE4swapERS3_", scope: !1263, file: !49, line: 1367, type: !1424, isLocal: false, isDefinition: false, scopeLine: 1367, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1424 = !DISubroutineType(types: !1425)
!1425 = !{null, !1269, !1309}
!1426 = !DISubprogram(name: "clear", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE5clearEv", scope: !1263, file: !49, line: 1385, type: !1267, isLocal: false, isDefinition: false, scopeLine: 1385, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1427 = !DISubprogram(name: "_M_fill_initialize", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE18_M_fill_initializeEmRKS1_", scope: !1263, file: !49, line: 1477, type: !1317, isLocal: false, isDefinition: false, scopeLine: 1477, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1428 = !DISubprogram(name: "_M_default_initialize", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE21_M_default_initializeEm", scope: !1263, file: !49, line: 1487, type: !1354, isLocal: false, isDefinition: false, scopeLine: 1487, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1429 = !DISubprogram(name: "_M_fill_assign", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE14_M_fill_assignEmRKS1_", scope: !1263, file: !49, line: 1529, type: !1317, isLocal: false, isDefinition: false, scopeLine: 1529, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1430 = !DISubprogram(name: "_M_fill_insert", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPS1_S3_EEmRKS1_", scope: !1263, file: !49, line: 1568, type: !1431, isLocal: false, isDefinition: false, scopeLine: 1568, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1431 = !DISubroutineType(types: !1432)
!1432 = !{null, !1269, !1262, !1260, !1282}
!1433 = !DISubprogram(name: "_M_default_append", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE17_M_default_appendEm", scope: !1263, file: !49, line: 1573, type: !1354, isLocal: false, isDefinition: false, scopeLine: 1573, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1434 = !DISubprogram(name: "_M_shrink_to_fit", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE16_M_shrink_to_fitEv", scope: !1263, file: !49, line: 1576, type: !1435, isLocal: false, isDefinition: false, scopeLine: 1576, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1435 = !DISubroutineType(types: !1436)
!1436 = !{!117, !1269}
!1437 = !DISubprogram(name: "_M_insert_rval", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE14_M_insert_rvalEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EEOS1_", scope: !1263, file: !49, line: 1625, type: !1409, isLocal: false, isDefinition: false, scopeLine: 1625, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1438 = !DISubprogram(name: "_M_emplace_aux", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE14_M_emplace_auxEN9__gnu_cxx17__normal_iteratorIPKS1_S3_EEOS1_", scope: !1263, file: !49, line: 1634, type: !1409, isLocal: false, isDefinition: false, scopeLine: 1634, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1439 = !DISubprogram(name: "_M_check_len", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE12_M_check_lenEmPKc", scope: !1263, file: !49, line: 1640, type: !1440, isLocal: false, isDefinition: false, scopeLine: 1640, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1440 = !DISubroutineType(types: !1441)
!1441 = !{!1442, !1330, !1260, !1443}
!1442 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !1263, file: !49, line: 374, baseType: !175)
!1443 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1444, size: 64)
!1444 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1171)
!1445 = !DISubprogram(name: "_M_erase_at_end", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE15_M_erase_at_endEPS1_", scope: !1263, file: !49, line: 1654, type: !1446, isLocal: false, isDefinition: false, scopeLine: 1654, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1446 = !DISubroutineType(types: !1447)
!1447 = !{null, !1269, !1448}
!1448 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1263, file: !49, line: 365, baseType: !57)
!1449 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE8_M_eraseEN9__gnu_cxx17__normal_iteratorIPS1_S3_EE", scope: !1263, file: !49, line: 1666, type: !1450, isLocal: false, isDefinition: false, scopeLine: 1666, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1450 = !DISubroutineType(types: !1451)
!1451 = !{!1262, !1269, !1262}
!1452 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE8_M_eraseEN9__gnu_cxx17__normal_iteratorIPS1_S3_EES7_", scope: !1263, file: !49, line: 1669, type: !1453, isLocal: false, isDefinition: false, scopeLine: 1669, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!1453 = !DISubroutineType(types: !1454)
!1454 = !{!1262, !1269, !1262, !1262}
!1455 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE14_M_move_assignEOS3_St17integral_constantIbLb1EE", scope: !1263, file: !49, line: 1677, type: !1456, isLocal: false, isDefinition: false, scopeLine: 1677, flags: DIFlagPrototyped, isOptimized: true)
!1456 = !DISubroutineType(types: !1457)
!1457 = !{null, !1269, !1293, !950}
!1458 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE14_M_move_assignEOS3_St17integral_constantIbLb0EE", scope: !1263, file: !49, line: 1688, type: !1459, isLocal: false, isDefinition: false, scopeLine: 1688, flags: DIFlagPrototyped, isOptimized: true)
!1459 = !DISubroutineType(types: !1460)
!1460 = !{null, !1269, !1293, !968}
!1461 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "__normal_iterator<std::pair<int, int> *, std::vector<std::pair<int, int>, std::allocator<std::pair<int, int> > > >", scope: !5, file: !879, line: 764, size: 64, flags: DIFlagTypePassByValue, elements: !1462, templateParams: !1516, identifier: "_ZTSN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEE")
!1462 = !{!1463, !1464, !1468, !1473, !1484, !1489, !1493, !1496, !1497, !1498, !1505, !1508, !1511, !1512, !1513}
!1463 = !DIDerivedType(tag: DW_TAG_member, name: "_M_current", scope: !1461, file: !879, line: 767, baseType: !70, size: 64, flags: DIFlagProtected)
!1464 = !DISubprogram(name: "__normal_iterator", scope: !1461, file: !879, line: 779, type: !1465, isLocal: false, isDefinition: false, scopeLine: 779, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1465 = !DISubroutineType(types: !1466)
!1466 = !{null, !1467}
!1467 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1461, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1468 = !DISubprogram(name: "__normal_iterator", scope: !1461, file: !879, line: 783, type: !1469, isLocal: false, isDefinition: false, scopeLine: 783, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!1469 = !DISubroutineType(types: !1470)
!1470 = !{null, !1467, !1471}
!1471 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1472, size: 64)
!1472 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !70)
!1473 = !DISubprogram(name: "operator*", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEdeEv", scope: !1461, file: !879, line: 796, type: !1474, isLocal: false, isDefinition: false, scopeLine: 796, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1474 = !DISubroutineType(types: !1475)
!1475 = !{!1476, !1482}
!1476 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !1461, file: !879, line: 776, baseType: !1477)
!1477 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !1479, file: !1478, line: 184, baseType: !110)
!1478 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_iterator_base_types.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1479 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "iterator_traits<std::pair<int, int> *>", scope: !13, file: !1478, line: 178, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !1480, identifier: "_ZTSSt15iterator_traitsIPSt4pairIiiEE")
!1480 = !{!1481}
!1481 = !DITemplateTypeParameter(name: "_Iterator", type: !70)
!1482 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1483, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1483 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1461)
!1484 = !DISubprogram(name: "operator->", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEptEv", scope: !1461, file: !879, line: 800, type: !1485, isLocal: false, isDefinition: false, scopeLine: 800, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1485 = !DISubroutineType(types: !1486)
!1486 = !{!1487, !1482}
!1487 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1461, file: !879, line: 777, baseType: !1488)
!1488 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1479, file: !1478, line: 183, baseType: !70)
!1489 = !DISubprogram(name: "operator++", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEppEv", scope: !1461, file: !879, line: 804, type: !1490, isLocal: false, isDefinition: false, scopeLine: 804, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1490 = !DISubroutineType(types: !1491)
!1491 = !{!1492, !1467}
!1492 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1461, size: 64)
!1493 = !DISubprogram(name: "operator++", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEppEi", scope: !1461, file: !879, line: 811, type: !1494, isLocal: false, isDefinition: false, scopeLine: 811, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1494 = !DISubroutineType(types: !1495)
!1495 = !{!1461, !1467, !93}
!1496 = !DISubprogram(name: "operator--", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEmmEv", scope: !1461, file: !879, line: 816, type: !1490, isLocal: false, isDefinition: false, scopeLine: 816, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1497 = !DISubprogram(name: "operator--", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEmmEi", scope: !1461, file: !879, line: 823, type: !1494, isLocal: false, isDefinition: false, scopeLine: 823, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1498 = !DISubprogram(name: "operator[]", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEixEl", scope: !1461, file: !879, line: 828, type: !1499, isLocal: false, isDefinition: false, scopeLine: 828, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1499 = !DISubroutineType(types: !1500)
!1500 = !{!1476, !1482, !1501}
!1501 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !1461, file: !879, line: 775, baseType: !1502)
!1502 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !1479, file: !1478, line: 182, baseType: !1503)
!1503 = !DIDerivedType(tag: DW_TAG_typedef, name: "ptrdiff_t", scope: !13, file: !176, line: 239, baseType: !1504)
!1504 = !DIBasicType(name: "long int", size: 64, encoding: DW_ATE_signed)
!1505 = !DISubprogram(name: "operator+=", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEpLEl", scope: !1461, file: !879, line: 832, type: !1506, isLocal: false, isDefinition: false, scopeLine: 832, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1506 = !DISubroutineType(types: !1507)
!1507 = !{!1492, !1467, !1501}
!1508 = !DISubprogram(name: "operator+", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEplEl", scope: !1461, file: !879, line: 836, type: !1509, isLocal: false, isDefinition: false, scopeLine: 836, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1509 = !DISubroutineType(types: !1510)
!1510 = !{!1461, !1482, !1501}
!1511 = !DISubprogram(name: "operator-=", linkageName: "_ZN9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEmIEl", scope: !1461, file: !879, line: 840, type: !1506, isLocal: false, isDefinition: false, scopeLine: 840, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1512 = !DISubprogram(name: "operator-", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEEmiEl", scope: !1461, file: !879, line: 844, type: !1509, isLocal: false, isDefinition: false, scopeLine: 844, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1513 = !DISubprogram(name: "base", linkageName: "_ZNK9__gnu_cxx17__normal_iteratorIPSt4pairIiiESt6vectorIS2_SaIS2_EEE4baseEv", scope: !1461, file: !879, line: 848, type: !1514, isLocal: false, isDefinition: false, scopeLine: 848, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1514 = !DISubroutineType(types: !1515)
!1515 = !{!1471, !1482}
!1516 = !{!1481, !1517}
!1517 = !DITemplateTypeParameter(name: "_Container", type: !1263)
!1518 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "move_iterator<std::pair<int, int> *>", scope: !13, file: !879, line: 1007, size: 64, flags: DIFlagTypePassByValue, elements: !1519, templateParams: !1480, identifier: "_ZTSSt13move_iteratorIPSt4pairIiiEE")
!1519 = !{!1520, !1521, !1525, !1529, !1534, !1542, !1546, !1550, !1553, !1554, !1555, !1559, !1562, !1563, !1564}
!1520 = !DIDerivedType(tag: DW_TAG_member, name: "_M_current", scope: !1518, file: !879, line: 1010, baseType: !70, size: 64, flags: DIFlagProtected)
!1521 = !DISubprogram(name: "move_iterator", scope: !1518, file: !879, line: 1029, type: !1522, isLocal: false, isDefinition: false, scopeLine: 1029, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1522 = !DISubroutineType(types: !1523)
!1523 = !{null, !1524}
!1524 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1518, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1525 = !DISubprogram(name: "move_iterator", scope: !1518, file: !879, line: 1033, type: !1526, isLocal: false, isDefinition: false, scopeLine: 1033, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!1526 = !DISubroutineType(types: !1527)
!1527 = !{null, !1524, !1528}
!1528 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator_type", scope: !1518, file: !879, line: 1016, baseType: !70)
!1529 = !DISubprogram(name: "base", linkageName: "_ZNKSt13move_iteratorIPSt4pairIiiEE4baseEv", scope: !1518, file: !879, line: 1042, type: !1530, isLocal: false, isDefinition: false, scopeLine: 1042, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1530 = !DISubroutineType(types: !1531)
!1531 = !{!1528, !1532}
!1532 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1533, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1533 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1518)
!1534 = !DISubprogram(name: "operator*", linkageName: "_ZNKSt13move_iteratorIPSt4pairIiiEEdeEv", scope: !1518, file: !879, line: 1046, type: !1535, isLocal: false, isDefinition: false, scopeLine: 1046, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1535 = !DISubroutineType(types: !1536)
!1536 = !{!1537, !1532}
!1537 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !1518, file: !879, line: 1026, baseType: !1538)
!1538 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !1539, file: !112, line: 1970, baseType: !106)
!1539 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "conditional<true, std::pair<int, int> &&, std::pair<int, int> &>", scope: !13, file: !112, line: 1969, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !1540, identifier: "_ZTSSt11conditionalILb1EOSt4pairIiiERS1_E")
!1540 = !{!116, !129, !1541}
!1541 = !DITemplateTypeParameter(name: "_Iffalse", type: !110)
!1542 = !DISubprogram(name: "operator->", linkageName: "_ZNKSt13move_iteratorIPSt4pairIiiEEptEv", scope: !1518, file: !879, line: 1050, type: !1543, isLocal: false, isDefinition: false, scopeLine: 1050, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1543 = !DISubroutineType(types: !1544)
!1544 = !{!1545, !1532}
!1545 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !1518, file: !879, line: 1021, baseType: !70)
!1546 = !DISubprogram(name: "operator++", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEppEv", scope: !1518, file: !879, line: 1054, type: !1547, isLocal: false, isDefinition: false, scopeLine: 1054, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1547 = !DISubroutineType(types: !1548)
!1548 = !{!1549, !1524}
!1549 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1518, size: 64)
!1550 = !DISubprogram(name: "operator++", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEppEi", scope: !1518, file: !879, line: 1061, type: !1551, isLocal: false, isDefinition: false, scopeLine: 1061, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1551 = !DISubroutineType(types: !1552)
!1552 = !{!1518, !1524, !93}
!1553 = !DISubprogram(name: "operator--", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEmmEv", scope: !1518, file: !879, line: 1069, type: !1547, isLocal: false, isDefinition: false, scopeLine: 1069, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1554 = !DISubprogram(name: "operator--", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEmmEi", scope: !1518, file: !879, line: 1076, type: !1551, isLocal: false, isDefinition: false, scopeLine: 1076, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1555 = !DISubprogram(name: "operator+", linkageName: "_ZNKSt13move_iteratorIPSt4pairIiiEEplEl", scope: !1518, file: !879, line: 1084, type: !1556, isLocal: false, isDefinition: false, scopeLine: 1084, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1556 = !DISubroutineType(types: !1557)
!1557 = !{!1518, !1532, !1558}
!1558 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !1518, file: !879, line: 1019, baseType: !1502)
!1559 = !DISubprogram(name: "operator+=", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEpLEl", scope: !1518, file: !879, line: 1088, type: !1560, isLocal: false, isDefinition: false, scopeLine: 1088, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1560 = !DISubroutineType(types: !1561)
!1561 = !{!1549, !1524, !1558}
!1562 = !DISubprogram(name: "operator-", linkageName: "_ZNKSt13move_iteratorIPSt4pairIiiEEmiEl", scope: !1518, file: !879, line: 1095, type: !1556, isLocal: false, isDefinition: false, scopeLine: 1095, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1563 = !DISubprogram(name: "operator-=", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEmIEl", scope: !1518, file: !879, line: 1099, type: !1560, isLocal: false, isDefinition: false, scopeLine: 1099, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1564 = !DISubprogram(name: "operator[]", linkageName: "_ZNKSt13move_iteratorIPSt4pairIiiEEixEl", scope: !1518, file: !879, line: 1106, type: !1565, isLocal: false, isDefinition: false, scopeLine: 1106, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1565 = !DISubroutineType(types: !1566)
!1566 = !{!1537, !1532, !1558}
!1567 = !{!1568}
!1568 = !DIGlobalVariableExpression(var: !1569, expr: !DIExpression())
!1569 = distinct !DIGlobalVariable(name: "gLsbReleasePath", linkageName: "_ZN7mozilla6widget3lsbL15gLsbReleasePathE", scope: !1570, file: !1, line: 17, type: !1443, isLocal: true, isDefinition: true)
!1570 = !DINamespace(name: "lsb", scope: !1571)
!1571 = !DINamespace(name: "widget", scope: !20)
!1572 = !{!1573, !1578, !1630, !1634, !1640, !1644, !1650, !1654, !1659, !1661, !1666, !1670, !1674, !1684, !1688, !1692, !1696, !1700, !1704, !1708, !1712, !1716, !1720, !1729, !1733, !1737, !1739, !1743, !1747, !1751, !1757, !1761, !1765, !1767, !1775, !1779, !1786, !1788, !1792, !1796, !1800, !1804, !1809, !1814, !1819, !1820, !1821, !1822, !1824, !1825, !1826, !1827, !1828, !1829, !1830, !1832, !1833, !1834, !1835, !1836, !1837, !1838, !1843, !1844, !1845, !1846, !1847, !1848, !1849, !1850, !1851, !1852, !1853, !1854, !1855, !1856, !1857, !1858, !1859, !1860, !1861, !1862, !1863, !1864, !1865, !1866, !1867, !1873, !1877, !1884, !1888, !1892, !1896, !1900, !1902, !1904, !1908, !1912, !1916, !1920, !1924, !1926, !1928, !1930, !1934, !1938, !1942, !1944, !1946, !1950, !1967, !1970, !1975, !1984, !1990, !1994, !1998, !2002, !2006, !2008, !2010, !2014, !2020, !2024, !2030, !2036, !2038, !2042, !2046, !2050, !2054, !2065, !2067, !2071, !2075, !2079, !2081, !2085, !2089, !2093, !2095, !2097, !2101, !2109, !2113, !2117, !2121, !2123, !2129, !2131, !2137, !2141, !2145, !2149, !2153, !2157, !2161, !2163, !2165, !2169, !2173, !2177, !2179, !2183, !2187, !2189, !2191, !2195, !2199, !2203, !2207, !2208, !2209, !2210, !2211, !2212, !2213, !2214, !2215, !2216, !2217, !2223, !2227, !2230, !2233, !2236, !2238, !2240, !2242, !2245, !2248, !2251, !2254, !2257, !2259, !2262, !2266, !2267, !2270, !2272, !2274, !2276, !2278, !2281, !2284, !2287, !2290, !2293, !2295, !2296, !2297, !2301, !2305, !2310, !2314, !2316, !2318, !2320, !2322, !2324, !2326, !2328, !2330, !2332, !2334, !2336, !2338, !2340, !2344, !2350, !2355, !2359, !2361, !2363, !2365, !2367, !2374, !2379, !2383, !2387, !2391, !2395, !2399, !2403, !2405, !2409, !2415, !2419, !2423, !2425, !2427, !2431, !2435, !2437, !2439, !2441, !2443, !2445, !2447, !2449, !2453, !2457, !2461, !2465, !2469, !2473, !2475, !2479, !2483, !2487, !2491, !2493, !2495, !2499, !2503, !2504, !2505, !2506, !2507, !2508, !2514, !2517, !2518, !2520, !2522, !2524, !2526, !2530, !2532, !2534, !2536, !2538, !2540, !2542, !2544, !2546, !2550, !2554, !2556, !2560, !2564, !2566, !2569, !2575, !2577, !2579, !2583, !2585, !2587, !2589, !2591, !2593, !2595, !2597, !2602, !2606, !2608, !2610, !2615, !2617, !2619, !2621, !2623, !2625, !2627, !2630, !2632, !2634, !2638, !2642, !2644, !2646, !2648, !2650, !2652, !2654, !2656, !2658, !2660, !2662, !2666, !2670, !2672, !2674, !2676, !2678, !2680, !2682, !2684, !2686, !2688, !2690, !2692, !2694, !2696, !2698, !2700, !2704, !2708, !2712, !2714, !2716, !2718, !2720, !2722, !2724, !2726, !2728, !2730, !2734, !2738, !2742, !2744, !2746, !2748, !2752, !2756, !2760, !2762, !2764, !2766, !2768, !2770, !2772, !2774, !2776, !2778, !2780, !2782, !2784, !2788, !2792, !2796, !2798, !2800, !2802, !2804, !2808, !2812, !2814, !2816, !2818, !2820, !2822, !2824, !2828, !2832, !2834, !2836, !2838, !2840, !2844, !2848, !2852, !2854, !2856, !2858, !2860, !2862, !2864, !2868, !2872, !2876, !2878, !2882, !2886, !2888, !2890, !2892, !2894, !2896, !2898}
!1573 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1574, file: !1577, line: 56)
!1574 = !DIDerivedType(tag: DW_TAG_typedef, name: "max_align_t", file: !1575, line: 40, baseType: !1576)
!1575 = !DIFile(filename: "/usr/lib/clang/7.0.0/include/__stddef_max_align_t.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1576 = !DICompositeType(tag: DW_TAG_structure_type, file: !1575, line: 35, flags: DIFlagFwdDecl, identifier: "_ZTS11max_align_t")
!1577 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cstddef", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1578 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1579, file: !1580, line: 57)
!1579 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "exception_ptr", scope: !1581, file: !1580, line: 79, size: 64, flags: DIFlagTypePassByReference, elements: !1582, identifier: "_ZTSNSt15__exception_ptr13exception_ptrE")
!1580 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/exception_ptr.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1581 = !DINamespace(name: "__exception_ptr", scope: !13)
!1582 = !{!1583, !1584, !1588, !1591, !1592, !1597, !1598, !1602, !1605, !1609, !1613, !1616, !1617, !1620, !1623}
!1583 = !DIDerivedType(tag: DW_TAG_member, name: "_M_exception_object", scope: !1579, file: !1580, line: 81, baseType: !506, size: 64)
!1584 = !DISubprogram(name: "exception_ptr", scope: !1579, file: !1580, line: 83, type: !1585, isLocal: false, isDefinition: false, scopeLine: 83, flags: DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!1585 = !DISubroutineType(types: !1586)
!1586 = !{null, !1587, !506}
!1587 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1579, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1588 = !DISubprogram(name: "_M_addref", linkageName: "_ZNSt15__exception_ptr13exception_ptr9_M_addrefEv", scope: !1579, file: !1580, line: 85, type: !1589, isLocal: false, isDefinition: false, scopeLine: 85, flags: DIFlagPrototyped, isOptimized: true)
!1589 = !DISubroutineType(types: !1590)
!1590 = !{null, !1587}
!1591 = !DISubprogram(name: "_M_release", linkageName: "_ZNSt15__exception_ptr13exception_ptr10_M_releaseEv", scope: !1579, file: !1580, line: 86, type: !1589, isLocal: false, isDefinition: false, scopeLine: 86, flags: DIFlagPrototyped, isOptimized: true)
!1592 = !DISubprogram(name: "_M_get", linkageName: "_ZNKSt15__exception_ptr13exception_ptr6_M_getEv", scope: !1579, file: !1580, line: 88, type: !1593, isLocal: false, isDefinition: false, scopeLine: 88, flags: DIFlagPrototyped, isOptimized: true)
!1593 = !DISubroutineType(types: !1594)
!1594 = !{!506, !1595}
!1595 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1596, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!1596 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1579)
!1597 = !DISubprogram(name: "exception_ptr", scope: !1579, file: !1580, line: 96, type: !1589, isLocal: false, isDefinition: false, scopeLine: 96, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1598 = !DISubprogram(name: "exception_ptr", scope: !1579, file: !1580, line: 98, type: !1599, isLocal: false, isDefinition: false, scopeLine: 98, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1599 = !DISubroutineType(types: !1600)
!1600 = !{null, !1587, !1601}
!1601 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1596, size: 64)
!1602 = !DISubprogram(name: "exception_ptr", scope: !1579, file: !1580, line: 101, type: !1603, isLocal: false, isDefinition: false, scopeLine: 101, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1603 = !DISubroutineType(types: !1604)
!1604 = !{null, !1587, !501}
!1605 = !DISubprogram(name: "exception_ptr", scope: !1579, file: !1580, line: 105, type: !1606, isLocal: false, isDefinition: false, scopeLine: 105, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1606 = !DISubroutineType(types: !1607)
!1607 = !{null, !1587, !1608}
!1608 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !1579, size: 64)
!1609 = !DISubprogram(name: "operator=", linkageName: "_ZNSt15__exception_ptr13exception_ptraSERKS0_", scope: !1579, file: !1580, line: 118, type: !1610, isLocal: false, isDefinition: false, scopeLine: 118, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1610 = !DISubroutineType(types: !1611)
!1611 = !{!1612, !1587, !1601}
!1612 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1579, size: 64)
!1613 = !DISubprogram(name: "operator=", linkageName: "_ZNSt15__exception_ptr13exception_ptraSEOS0_", scope: !1579, file: !1580, line: 122, type: !1614, isLocal: false, isDefinition: false, scopeLine: 122, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1614 = !DISubroutineType(types: !1615)
!1615 = !{!1612, !1587, !1608}
!1616 = !DISubprogram(name: "~exception_ptr", scope: !1579, file: !1580, line: 129, type: !1589, isLocal: false, isDefinition: false, scopeLine: 129, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1617 = !DISubprogram(name: "swap", linkageName: "_ZNSt15__exception_ptr13exception_ptr4swapERS0_", scope: !1579, file: !1580, line: 132, type: !1618, isLocal: false, isDefinition: false, scopeLine: 132, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1618 = !DISubroutineType(types: !1619)
!1619 = !{null, !1587, !1612}
!1620 = !DISubprogram(name: "operator bool", linkageName: "_ZNKSt15__exception_ptr13exception_ptrcvbEv", scope: !1579, file: !1580, line: 144, type: !1621, isLocal: false, isDefinition: false, scopeLine: 144, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!1621 = !DISubroutineType(types: !1622)
!1622 = !{!117, !1595}
!1623 = !DISubprogram(name: "__cxa_exception_type", linkageName: "_ZNKSt15__exception_ptr13exception_ptr20__cxa_exception_typeEv", scope: !1579, file: !1580, line: 153, type: !1624, isLocal: false, isDefinition: false, scopeLine: 153, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!1624 = !DISubroutineType(types: !1625)
!1625 = !{!1626, !1595}
!1626 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1627, size: 64)
!1627 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1628)
!1628 = !DICompositeType(tag: DW_TAG_class_type, name: "type_info", scope: !13, file: !1629, line: 88, flags: DIFlagFwdDecl, identifier: "_ZTSSt9type_info")
!1629 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/typeinfo", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1630 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !1581, entity: !1631, file: !1580, line: 73)
!1631 = !DISubprogram(name: "rethrow_exception", linkageName: "_ZSt17rethrow_exceptionNSt15__exception_ptr13exception_ptrE", scope: !13, file: !1580, line: 69, type: !1632, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1632 = !DISubroutineType(types: !1633)
!1633 = !{null, !1579}
!1634 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1635, file: !1639, line: 52)
!1635 = !DISubprogram(name: "abs", scope: !1636, file: !1636, line: 837, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1636 = !DIFile(filename: "/usr/include/stdlib.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1637 = !DISubroutineType(types: !1638)
!1638 = !{!93, !93}
!1639 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/std_abs.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1640 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1641, file: !1643, line: 127)
!1641 = !DIDerivedType(tag: DW_TAG_typedef, name: "div_t", file: !1636, line: 62, baseType: !1642)
!1642 = !DICompositeType(tag: DW_TAG_structure_type, file: !1636, line: 58, flags: DIFlagFwdDecl, identifier: "_ZTS5div_t")
!1643 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cstdlib", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1644 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1645, file: !1643, line: 128)
!1645 = !DIDerivedType(tag: DW_TAG_typedef, name: "ldiv_t", file: !1636, line: 70, baseType: !1646)
!1646 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !1636, line: 66, size: 128, flags: DIFlagTypePassByValue, elements: !1647, identifier: "_ZTS6ldiv_t")
!1647 = !{!1648, !1649}
!1648 = !DIDerivedType(tag: DW_TAG_member, name: "quot", scope: !1646, file: !1636, line: 68, baseType: !1504, size: 64)
!1649 = !DIDerivedType(tag: DW_TAG_member, name: "rem", scope: !1646, file: !1636, line: 69, baseType: !1504, size: 64, offset: 64)
!1650 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1651, file: !1643, line: 130)
!1651 = !DISubprogram(name: "abort", scope: !1636, file: !1636, line: 588, type: !1652, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1652 = !DISubroutineType(types: !1653)
!1653 = !{null}
!1654 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1655, file: !1643, line: 134)
!1655 = !DISubprogram(name: "atexit", scope: !1636, file: !1636, line: 592, type: !1656, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1656 = !DISubroutineType(types: !1657)
!1657 = !{!93, !1658}
!1658 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1652, size: 64)
!1659 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1660, file: !1643, line: 137)
!1660 = !DISubprogram(name: "at_quick_exit", scope: !1636, file: !1636, line: 597, type: !1656, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1661 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1662, file: !1643, line: 140)
!1662 = !DISubprogram(name: "atof", scope: !1636, file: !1636, line: 101, type: !1663, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1663 = !DISubroutineType(types: !1664)
!1664 = !{!1665, !1443}
!1665 = !DIBasicType(name: "double", size: 64, encoding: DW_ATE_float)
!1666 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1667, file: !1643, line: 141)
!1667 = !DISubprogram(name: "atoi", scope: !1636, file: !1636, line: 104, type: !1668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1668 = !DISubroutineType(types: !1669)
!1669 = !{!93, !1443}
!1670 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1671, file: !1643, line: 142)
!1671 = !DISubprogram(name: "atol", scope: !1636, file: !1636, line: 107, type: !1672, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1672 = !DISubroutineType(types: !1673)
!1673 = !{!1504, !1443}
!1674 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1675, file: !1643, line: 143)
!1675 = !DISubprogram(name: "bsearch", scope: !1636, file: !1636, line: 817, type: !1676, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1676 = !DISubroutineType(types: !1677)
!1677 = !{!506, !178, !178, !1678, !1678, !1680}
!1678 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_t", file: !1679, line: 62, baseType: !177)
!1679 = !DIFile(filename: "/usr/lib/clang/7.0.0/include/stddef.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1680 = !DIDerivedType(tag: DW_TAG_typedef, name: "__compar_fn_t", file: !1636, line: 805, baseType: !1681)
!1681 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1682, size: 64)
!1682 = !DISubroutineType(types: !1683)
!1683 = !{!93, !178, !178}
!1684 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1685, file: !1643, line: 144)
!1685 = !DISubprogram(name: "calloc", scope: !1636, file: !1636, line: 541, type: !1686, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1686 = !DISubroutineType(types: !1687)
!1687 = !{!506, !1678, !1678}
!1688 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1689, file: !1643, line: 145)
!1689 = !DISubprogram(name: "div", scope: !1636, file: !1636, line: 849, type: !1690, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1690 = !DISubroutineType(types: !1691)
!1691 = !{!1641, !93, !93}
!1692 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1693, file: !1643, line: 146)
!1693 = !DISubprogram(name: "exit", scope: !1636, file: !1636, line: 614, type: !1694, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1694 = !DISubroutineType(types: !1695)
!1695 = !{null, !93}
!1696 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1697, file: !1643, line: 147)
!1697 = !DISubprogram(name: "free", scope: !1636, file: !1636, line: 563, type: !1698, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1698 = !DISubroutineType(types: !1699)
!1699 = !{null, !506}
!1700 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1701, file: !1643, line: 148)
!1701 = !DISubprogram(name: "getenv", scope: !1636, file: !1636, line: 631, type: !1702, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1702 = !DISubroutineType(types: !1703)
!1703 = !{!1170, !1443}
!1704 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1705, file: !1643, line: 149)
!1705 = !DISubprogram(name: "labs", scope: !1636, file: !1636, line: 838, type: !1706, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1706 = !DISubroutineType(types: !1707)
!1707 = !{!1504, !1504}
!1708 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1709, file: !1643, line: 150)
!1709 = !DISubprogram(name: "ldiv", scope: !1636, file: !1636, line: 851, type: !1710, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1710 = !DISubroutineType(types: !1711)
!1711 = !{!1645, !1504, !1504}
!1712 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1713, file: !1643, line: 151)
!1713 = !DISubprogram(name: "malloc", scope: !1636, file: !1636, line: 539, type: !1714, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1714 = !DISubroutineType(types: !1715)
!1715 = !{!506, !1678}
!1716 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1717, file: !1643, line: 153)
!1717 = !DISubprogram(name: "mblen", scope: !1636, file: !1636, line: 919, type: !1718, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1718 = !DISubroutineType(types: !1719)
!1719 = !{!93, !1443, !1678}
!1720 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1721, file: !1643, line: 154)
!1721 = !DISubprogram(name: "mbstowcs", scope: !1722, file: !1722, line: 113, type: !1723, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1722 = !DIFile(filename: "/usr/include/bits/stdlib.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1723 = !DISubroutineType(types: !1724)
!1724 = !{!1678, !1725, !1728, !1678}
!1725 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1726)
!1726 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1727, size: 64)
!1727 = !DIBasicType(name: "wchar_t", size: 32, encoding: DW_ATE_signed)
!1728 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1443)
!1729 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1730, file: !1643, line: 155)
!1730 = !DISubprogram(name: "mbtowc", scope: !1636, file: !1636, line: 922, type: !1731, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1731 = !DISubroutineType(types: !1732)
!1732 = !{!93, !1725, !1728, !1678}
!1733 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1734, file: !1643, line: 157)
!1734 = !DISubprogram(name: "qsort", scope: !1636, file: !1636, line: 827, type: !1735, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1735 = !DISubroutineType(types: !1736)
!1736 = !{null, !506, !1678, !1678, !1680}
!1737 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1738, file: !1643, line: 160)
!1738 = !DISubprogram(name: "quick_exit", scope: !1636, file: !1636, line: 620, type: !1694, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1739 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1740, file: !1643, line: 163)
!1740 = !DISubprogram(name: "rand", scope: !1636, file: !1636, line: 453, type: !1741, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1741 = !DISubroutineType(types: !1742)
!1742 = !{!93}
!1743 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1744, file: !1643, line: 164)
!1744 = !DISubprogram(name: "realloc", scope: !1636, file: !1636, line: 549, type: !1745, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1745 = !DISubroutineType(types: !1746)
!1746 = !{!506, !506, !1678}
!1747 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1748, file: !1643, line: 165)
!1748 = !DISubprogram(name: "srand", scope: !1636, file: !1636, line: 455, type: !1749, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1749 = !DISubroutineType(types: !1750)
!1750 = !{null, !6}
!1751 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1752, file: !1643, line: 166)
!1752 = !DISubprogram(name: "strtod", scope: !1636, file: !1636, line: 117, type: !1753, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1753 = !DISubroutineType(types: !1754)
!1754 = !{!1665, !1728, !1755}
!1755 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1756)
!1756 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1170, size: 64)
!1757 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1758, file: !1643, line: 167)
!1758 = !DISubprogram(name: "strtol", scope: !1636, file: !1636, line: 176, type: !1759, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1759 = !DISubroutineType(types: !1760)
!1760 = !{!1504, !1728, !1755, !93}
!1761 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1762, file: !1643, line: 168)
!1762 = !DISubprogram(name: "strtoul", scope: !1636, file: !1636, line: 180, type: !1763, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1763 = !DISubroutineType(types: !1764)
!1764 = !{!177, !1728, !1755, !93}
!1765 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1766, file: !1643, line: 169)
!1766 = !DISubprogram(name: "system", scope: !1636, file: !1636, line: 781, type: !1668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1767 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1768, file: !1643, line: 171)
!1768 = !DISubprogram(name: "wcstombs", scope: !1722, file: !1722, line: 144, type: !1769, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1769 = !DISubroutineType(types: !1770)
!1770 = !{!1678, !1771, !1772, !1678}
!1771 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1170)
!1772 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1773)
!1773 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1774, size: 64)
!1774 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1727)
!1775 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1776, file: !1643, line: 172)
!1776 = !DISubprogram(name: "wctomb", scope: !1722, file: !1722, line: 83, type: !1777, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1777 = !DISubroutineType(types: !1778)
!1778 = !{!93, !1170, !1727}
!1779 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1780, file: !1643, line: 200)
!1780 = !DIDerivedType(tag: DW_TAG_typedef, name: "lldiv_t", file: !1636, line: 80, baseType: !1781)
!1781 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !1636, line: 76, size: 128, flags: DIFlagTypePassByValue, elements: !1782, identifier: "_ZTS7lldiv_t")
!1782 = !{!1783, !1785}
!1783 = !DIDerivedType(tag: DW_TAG_member, name: "quot", scope: !1781, file: !1636, line: 78, baseType: !1784, size: 64)
!1784 = !DIBasicType(name: "long long int", size: 64, encoding: DW_ATE_signed)
!1785 = !DIDerivedType(tag: DW_TAG_member, name: "rem", scope: !1781, file: !1636, line: 79, baseType: !1784, size: 64, offset: 64)
!1786 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1787, file: !1643, line: 206)
!1787 = !DISubprogram(name: "_Exit", scope: !1636, file: !1636, line: 626, type: !1694, isLocal: false, isDefinition: false, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true)
!1788 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1789, file: !1643, line: 210)
!1789 = !DISubprogram(name: "llabs", scope: !1636, file: !1636, line: 841, type: !1790, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1790 = !DISubroutineType(types: !1791)
!1791 = !{!1784, !1784}
!1792 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1793, file: !1643, line: 216)
!1793 = !DISubprogram(name: "lldiv", scope: !1636, file: !1636, line: 855, type: !1794, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1794 = !DISubroutineType(types: !1795)
!1795 = !{!1780, !1784, !1784}
!1796 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1797, file: !1643, line: 227)
!1797 = !DISubprogram(name: "atoll", scope: !1636, file: !1636, line: 112, type: !1798, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1798 = !DISubroutineType(types: !1799)
!1799 = !{!1784, !1443}
!1800 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1801, file: !1643, line: 228)
!1801 = !DISubprogram(name: "strtoll", scope: !1636, file: !1636, line: 200, type: !1802, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1802 = !DISubroutineType(types: !1803)
!1803 = !{!1784, !1728, !1755, !93}
!1804 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1805, file: !1643, line: 229)
!1805 = !DISubprogram(name: "strtoull", scope: !1636, file: !1636, line: 205, type: !1806, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1806 = !DISubroutineType(types: !1807)
!1807 = !{!1808, !1728, !1755, !93}
!1808 = !DIBasicType(name: "long long unsigned int", size: 64, encoding: DW_ATE_unsigned)
!1809 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1810, file: !1643, line: 231)
!1810 = !DISubprogram(name: "strtof", scope: !1636, file: !1636, line: 123, type: !1811, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1811 = !DISubroutineType(types: !1812)
!1812 = !{!1813, !1728, !1755}
!1813 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
!1814 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1815, file: !1643, line: 232)
!1815 = !DISubprogram(name: "strtold", scope: !1636, file: !1636, line: 126, type: !1816, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1816 = !DISubroutineType(types: !1817)
!1817 = !{!1818, !1728, !1755}
!1818 = !DIBasicType(name: "long double", size: 128, encoding: DW_ATE_float)
!1819 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1780, file: !1643, line: 240)
!1820 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1787, file: !1643, line: 242)
!1821 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1789, file: !1643, line: 244)
!1822 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1823, file: !1643, line: 245)
!1823 = !DISubprogram(name: "div", linkageName: "_ZN9__gnu_cxx3divExx", scope: !5, file: !1643, line: 213, type: !1794, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1824 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1793, file: !1643, line: 246)
!1825 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1797, file: !1643, line: 248)
!1826 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1810, file: !1643, line: 249)
!1827 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1801, file: !1643, line: 250)
!1828 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1805, file: !1643, line: 251)
!1829 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1815, file: !1643, line: 252)
!1830 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1651, file: !1831, line: 38)
!1831 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/stdlib.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1832 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1655, file: !1831, line: 39)
!1833 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1693, file: !1831, line: 40)
!1834 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1660, file: !1831, line: 43)
!1835 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1738, file: !1831, line: 46)
!1836 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1641, file: !1831, line: 51)
!1837 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1645, file: !1831, line: 52)
!1838 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1839, file: !1831, line: 54)
!1839 = !DISubprogram(name: "abs", linkageName: "_ZSt3absg", scope: !13, file: !1639, line: 102, type: !1840, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1840 = !DISubroutineType(types: !1841)
!1841 = !{!1842, !1842}
!1842 = !DIBasicType(name: "__float128", size: 128, encoding: DW_ATE_float)
!1843 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1662, file: !1831, line: 55)
!1844 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1667, file: !1831, line: 56)
!1845 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1671, file: !1831, line: 57)
!1846 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1675, file: !1831, line: 58)
!1847 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1685, file: !1831, line: 59)
!1848 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1823, file: !1831, line: 60)
!1849 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1697, file: !1831, line: 61)
!1850 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1701, file: !1831, line: 62)
!1851 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1705, file: !1831, line: 63)
!1852 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1709, file: !1831, line: 64)
!1853 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1713, file: !1831, line: 65)
!1854 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1717, file: !1831, line: 67)
!1855 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1721, file: !1831, line: 68)
!1856 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1730, file: !1831, line: 69)
!1857 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1734, file: !1831, line: 71)
!1858 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1740, file: !1831, line: 72)
!1859 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1744, file: !1831, line: 73)
!1860 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1748, file: !1831, line: 74)
!1861 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1752, file: !1831, line: 75)
!1862 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1758, file: !1831, line: 76)
!1863 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1762, file: !1831, line: 77)
!1864 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1766, file: !1831, line: 78)
!1865 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1768, file: !1831, line: 80)
!1866 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !0, entity: !1776, file: !1831, line: 81)
!1867 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1868, file: !1872, line: 75)
!1868 = !DISubprogram(name: "memchr", scope: !1869, file: !1869, line: 90, type: !1870, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1869 = !DIFile(filename: "/usr/include/string.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1870 = !DISubroutineType(types: !1871)
!1871 = !{!506, !178, !93, !1678}
!1872 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cstring", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1873 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1874, file: !1872, line: 76)
!1874 = !DISubprogram(name: "memcmp", scope: !1869, file: !1869, line: 63, type: !1875, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1875 = !DISubroutineType(types: !1876)
!1876 = !{!93, !178, !178, !1678}
!1877 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1878, file: !1872, line: 77)
!1878 = !DISubprogram(name: "memcpy", scope: !1879, file: !1879, line: 31, type: !1880, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1879 = !DIFile(filename: "/usr/include/bits/string_fortified.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1880 = !DISubroutineType(types: !1881)
!1881 = !{!506, !1882, !1883, !1678}
!1882 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !506)
!1883 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !178)
!1884 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1885, file: !1872, line: 78)
!1885 = !DISubprogram(name: "memmove", scope: !1879, file: !1879, line: 38, type: !1886, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1886 = !DISubroutineType(types: !1887)
!1887 = !{!506, !506, !178, !1678}
!1888 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1889, file: !1872, line: 79)
!1889 = !DISubprogram(name: "memset", scope: !1879, file: !1879, line: 59, type: !1890, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1890 = !DISubroutineType(types: !1891)
!1891 = !{!506, !506, !93, !1678}
!1892 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1893, file: !1872, line: 80)
!1893 = !DISubprogram(name: "strcat", scope: !1879, file: !1879, line: 126, type: !1894, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1894 = !DISubroutineType(types: !1895)
!1895 = !{!1170, !1771, !1728}
!1896 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1897, file: !1872, line: 81)
!1897 = !DISubprogram(name: "strcmp", scope: !1869, file: !1869, line: 136, type: !1898, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1898 = !DISubroutineType(types: !1899)
!1899 = !{!93, !1443, !1443}
!1900 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1901, file: !1872, line: 82)
!1901 = !DISubprogram(name: "strcoll", scope: !1869, file: !1869, line: 143, type: !1898, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1902 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1903, file: !1872, line: 83)
!1903 = !DISubprogram(name: "strcpy", scope: !1879, file: !1879, line: 88, type: !1894, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1904 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1905, file: !1872, line: 84)
!1905 = !DISubprogram(name: "strcspn", scope: !1869, file: !1869, line: 272, type: !1906, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1906 = !DISubroutineType(types: !1907)
!1907 = !{!1678, !1443, !1443}
!1908 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1909, file: !1872, line: 85)
!1909 = !DISubprogram(name: "strerror", scope: !1869, file: !1869, line: 396, type: !1910, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1910 = !DISubroutineType(types: !1911)
!1911 = !{!1170, !93}
!1912 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1913, file: !1872, line: 86)
!1913 = !DISubprogram(name: "strlen", scope: !1869, file: !1869, line: 384, type: !1914, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1914 = !DISubroutineType(types: !1915)
!1915 = !{!1678, !1443}
!1916 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1917, file: !1872, line: 87)
!1917 = !DISubprogram(name: "strncat", scope: !1879, file: !1879, line: 133, type: !1918, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1918 = !DISubroutineType(types: !1919)
!1919 = !{!1170, !1771, !1728, !1678}
!1920 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1921, file: !1872, line: 88)
!1921 = !DISubprogram(name: "strncmp", scope: !1869, file: !1869, line: 139, type: !1922, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1922 = !DISubroutineType(types: !1923)
!1923 = !{!93, !1443, !1443, !1678}
!1924 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1925, file: !1872, line: 89)
!1925 = !DISubprogram(name: "strncpy", scope: !1879, file: !1879, line: 103, type: !1918, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1926 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1927, file: !1872, line: 90)
!1927 = !DISubprogram(name: "strspn", scope: !1869, file: !1869, line: 276, type: !1906, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1928 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1929, file: !1872, line: 91)
!1929 = !DISubprogram(name: "strtok", scope: !1869, file: !1869, line: 335, type: !1894, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1930 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1931, file: !1872, line: 92)
!1931 = !DISubprogram(name: "strxfrm", scope: !1869, file: !1869, line: 146, type: !1932, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1932 = !DISubroutineType(types: !1933)
!1933 = !{!1678, !1771, !1728, !1678}
!1934 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1935, file: !1872, line: 93)
!1935 = !DISubprogram(name: "strchr", scope: !1869, file: !1869, line: 225, type: !1936, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1936 = !DISubroutineType(types: !1937)
!1937 = !{!1170, !1443, !93}
!1938 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1939, file: !1872, line: 94)
!1939 = !DISubprogram(name: "strpbrk", scope: !1869, file: !1869, line: 302, type: !1940, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1940 = !DISubroutineType(types: !1941)
!1941 = !{!1170, !1443, !1443}
!1942 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1943, file: !1872, line: 95)
!1943 = !DISubprogram(name: "strrchr", scope: !1869, file: !1869, line: 252, type: !1936, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1944 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1945, file: !1872, line: 96)
!1945 = !DISubprogram(name: "strstr", scope: !1869, file: !1869, line: 329, type: !1940, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1946 = !DIImportedEntity(tag: DW_TAG_imported_module, scope: !1947, entity: !1948, file: !1949, line: 58)
!1947 = !DINamespace(name: "__gnu_debug", scope: null)
!1948 = !DINamespace(name: "__debug", scope: !13)
!1949 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/debug/debug.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1950 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1951, file: !1966, line: 64)
!1951 = !DIDerivedType(tag: DW_TAG_typedef, name: "mbstate_t", file: !1952, line: 6, baseType: !1953)
!1952 = !DIFile(filename: "/usr/include/bits/types/mbstate_t.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1953 = !DIDerivedType(tag: DW_TAG_typedef, name: "__mbstate_t", file: !1954, line: 21, baseType: !1955)
!1954 = !DIFile(filename: "/usr/include/bits/types/__mbstate_t.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1955 = distinct !DICompositeType(tag: DW_TAG_structure_type, file: !1954, line: 13, size: 64, flags: DIFlagTypePassByValue, elements: !1956, identifier: "_ZTS11__mbstate_t")
!1956 = !{!1957, !1958}
!1957 = !DIDerivedType(tag: DW_TAG_member, name: "__count", scope: !1955, file: !1954, line: 15, baseType: !93, size: 32)
!1958 = !DIDerivedType(tag: DW_TAG_member, name: "__value", scope: !1955, file: !1954, line: 20, baseType: !1959, size: 32, offset: 32)
!1959 = distinct !DICompositeType(tag: DW_TAG_union_type, scope: !1955, file: !1954, line: 16, size: 32, flags: DIFlagTypePassByValue, elements: !1960, identifier: "_ZTSN11__mbstate_tUt_E")
!1960 = !{!1961, !1962}
!1961 = !DIDerivedType(tag: DW_TAG_member, name: "__wch", scope: !1959, file: !1954, line: 18, baseType: !6, size: 32)
!1962 = !DIDerivedType(tag: DW_TAG_member, name: "__wchb", scope: !1959, file: !1954, line: 19, baseType: !1963, size: 32)
!1963 = !DICompositeType(tag: DW_TAG_array_type, baseType: !1171, size: 32, elements: !1964)
!1964 = !{!1965}
!1965 = !DISubrange(count: 4)
!1966 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cwchar", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1967 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1968, file: !1966, line: 139)
!1968 = !DIDerivedType(tag: DW_TAG_typedef, name: "wint_t", file: !1969, line: 20, baseType: !6)
!1969 = !DIFile(filename: "/usr/include/bits/types/wint_t.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1970 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1971, file: !1966, line: 141)
!1971 = !DISubprogram(name: "btowc", scope: !1972, file: !1972, line: 284, type: !1973, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1972 = !DIFile(filename: "/usr/include/wchar.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1973 = !DISubroutineType(types: !1974)
!1974 = !{!1968, !93}
!1975 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1976, file: !1966, line: 142)
!1976 = !DISubprogram(name: "fgetwc", scope: !1972, file: !1972, line: 727, type: !1977, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1977 = !DISubroutineType(types: !1978)
!1978 = !{!1968, !1979}
!1979 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1980, size: 64)
!1980 = !DIDerivedType(tag: DW_TAG_typedef, name: "__FILE", file: !1981, line: 5, baseType: !1982)
!1981 = !DIFile(filename: "/usr/include/bits/types/__FILE.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1982 = !DICompositeType(tag: DW_TAG_structure_type, name: "_IO_FILE", file: !1983, line: 49, flags: DIFlagFwdDecl, identifier: "_ZTS8_IO_FILE")
!1983 = !DIFile(filename: "/usr/include/bits/types/struct_FILE.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1984 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1985, file: !1966, line: 143)
!1985 = !DISubprogram(name: "fgetws", scope: !1986, file: !1986, line: 384, type: !1987, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1986 = !DIFile(filename: "/usr/include/bits/wchar2.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!1987 = !DISubroutineType(types: !1988)
!1988 = !{!1726, !1725, !93, !1989}
!1989 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !1979)
!1990 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1991, file: !1966, line: 144)
!1991 = !DISubprogram(name: "fputwc", scope: !1972, file: !1972, line: 741, type: !1992, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1992 = !DISubroutineType(types: !1993)
!1993 = !{!1968, !1727, !1979}
!1994 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1995, file: !1966, line: 145)
!1995 = !DISubprogram(name: "fputws", scope: !1972, file: !1972, line: 763, type: !1996, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!1996 = !DISubroutineType(types: !1997)
!1997 = !{!93, !1772, !1989}
!1998 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1999, file: !1966, line: 146)
!1999 = !DISubprogram(name: "fwide", scope: !1972, file: !1972, line: 573, type: !2000, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2000 = !DISubroutineType(types: !2001)
!2001 = !{!93, !1979, !93}
!2002 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2003, file: !1966, line: 147)
!2003 = !DISubprogram(name: "fwprintf", scope: !1972, file: !1972, line: 580, type: !2004, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2004 = !DISubroutineType(types: !2005)
!2005 = !{!93, !1989, !1772, null}
!2006 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2007, file: !1966, line: 148)
!2007 = !DISubprogram(name: "fwscanf", scope: !1972, file: !1972, line: 621, type: !2004, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2008 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2009, file: !1966, line: 149)
!2009 = !DISubprogram(name: "getwc", scope: !1972, file: !1972, line: 728, type: !1977, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2010 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2011, file: !1966, line: 150)
!2011 = !DISubprogram(name: "getwchar", scope: !1972, file: !1972, line: 734, type: !2012, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2012 = !DISubroutineType(types: !2013)
!2013 = !{!1968}
!2014 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2015, file: !1966, line: 151)
!2015 = !DISubprogram(name: "mbrlen", scope: !1972, file: !1972, line: 307, type: !2016, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2016 = !DISubroutineType(types: !2017)
!2017 = !{!1678, !1728, !1678, !2018}
!2018 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2019)
!2019 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1951, size: 64)
!2020 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2021, file: !1966, line: 152)
!2021 = !DISubprogram(name: "mbrtowc", scope: !1972, file: !1972, line: 296, type: !2022, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2022 = !DISubroutineType(types: !2023)
!2023 = !{!1678, !1725, !1728, !1678, !2018}
!2024 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2025, file: !1966, line: 153)
!2025 = !DISubprogram(name: "mbsinit", scope: !1972, file: !1972, line: 292, type: !2026, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2026 = !DISubroutineType(types: !2027)
!2027 = !{!93, !2028}
!2028 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2029, size: 64)
!2029 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1951)
!2030 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2031, file: !1966, line: 154)
!2031 = !DISubprogram(name: "mbsrtowcs", scope: !1986, file: !1986, line: 474, type: !2032, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2032 = !DISubroutineType(types: !2033)
!2033 = !{!1678, !1725, !2034, !1678, !2018}
!2034 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2035)
!2035 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1443, size: 64)
!2036 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2037, file: !1966, line: 155)
!2037 = !DISubprogram(name: "putwc", scope: !1972, file: !1972, line: 742, type: !1992, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2038 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2039, file: !1966, line: 156)
!2039 = !DISubprogram(name: "putwchar", scope: !1972, file: !1972, line: 748, type: !2040, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2040 = !DISubroutineType(types: !2041)
!2041 = !{!1968, !1727}
!2042 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2043, file: !1966, line: 158)
!2043 = !DISubprogram(name: "swprintf", scope: !1972, file: !1972, line: 590, type: !2044, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2044 = !DISubroutineType(types: !2045)
!2045 = !{!93, !1725, !1678, !1772, null}
!2046 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2047, file: !1966, line: 160)
!2047 = !DISubprogram(name: "swscanf", scope: !1972, file: !1972, line: 631, type: !2048, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2048 = !DISubroutineType(types: !2049)
!2049 = !{!93, !1772, !1772, null}
!2050 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2051, file: !1966, line: 161)
!2051 = !DISubprogram(name: "ungetwc", scope: !1972, file: !1972, line: 771, type: !2052, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2052 = !DISubroutineType(types: !2053)
!2053 = !{!1968, !1968, !1979}
!2054 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2055, file: !1966, line: 162)
!2055 = !DISubprogram(name: "vfwprintf", scope: !1986, file: !1986, line: 364, type: !2056, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2056 = !DISubroutineType(types: !2057)
!2057 = !{!93, !1989, !1772, !2058}
!2058 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2059, size: 64)
!2059 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__va_list_tag", file: !1, size: 192, flags: DIFlagTypePassByValue, elements: !2060, identifier: "_ZTS13__va_list_tag")
!2060 = !{!2061, !2062, !2063, !2064}
!2061 = !DIDerivedType(tag: DW_TAG_member, name: "gp_offset", scope: !2059, file: !1, baseType: !6, size: 32)
!2062 = !DIDerivedType(tag: DW_TAG_member, name: "fp_offset", scope: !2059, file: !1, baseType: !6, size: 32, offset: 32)
!2063 = !DIDerivedType(tag: DW_TAG_member, name: "overflow_arg_area", scope: !2059, file: !1, baseType: !506, size: 64, offset: 64)
!2064 = !DIDerivedType(tag: DW_TAG_member, name: "reg_save_area", scope: !2059, file: !1, baseType: !506, size: 64, offset: 128)
!2065 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2066, file: !1966, line: 164)
!2066 = !DISubprogram(name: "vfwscanf", scope: !1972, file: !1972, line: 673, type: !2056, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2067 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2068, file: !1966, line: 167)
!2068 = !DISubprogram(name: "vswprintf", scope: !1986, file: !1986, line: 315, type: !2069, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2069 = !DISubroutineType(types: !2070)
!2070 = !{!93, !1725, !1678, !1772, !2058}
!2071 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2072, file: !1966, line: 170)
!2072 = !DISubprogram(name: "vswscanf", scope: !1972, file: !1972, line: 685, type: !2073, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2073 = !DISubroutineType(types: !2074)
!2074 = !{!93, !1772, !1772, !2058}
!2075 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2076, file: !1966, line: 172)
!2076 = !DISubprogram(name: "vwprintf", scope: !1986, file: !1986, line: 358, type: !2077, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2077 = !DISubroutineType(types: !2078)
!2078 = !{!93, !1772, !2058}
!2079 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2080, file: !1966, line: 174)
!2080 = !DISubprogram(name: "vwscanf", scope: !1972, file: !1972, line: 681, type: !2077, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2081 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2082, file: !1966, line: 176)
!2082 = !DISubprogram(name: "wcrtomb", scope: !1986, file: !1986, line: 440, type: !2083, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2083 = !DISubroutineType(types: !2084)
!2084 = !{!1678, !1771, !1727, !2018}
!2085 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2086, file: !1966, line: 177)
!2086 = !DISubprogram(name: "wcscat", scope: !1986, file: !1986, line: 246, type: !2087, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2087 = !DISubroutineType(types: !2088)
!2088 = !{!1726, !1725, !1772}
!2089 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2090, file: !1966, line: 178)
!2090 = !DISubprogram(name: "wcscmp", scope: !1972, file: !1972, line: 106, type: !2091, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2091 = !DISubroutineType(types: !2092)
!2092 = !{!93, !1773, !1773}
!2093 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2094, file: !1966, line: 179)
!2094 = !DISubprogram(name: "wcscoll", scope: !1972, file: !1972, line: 131, type: !2091, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2095 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2096, file: !1966, line: 180)
!2096 = !DISubprogram(name: "wcscpy", scope: !1986, file: !1986, line: 152, type: !2087, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2097 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2098, file: !1966, line: 181)
!2098 = !DISubprogram(name: "wcscspn", scope: !1972, file: !1972, line: 187, type: !2099, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2099 = !DISubroutineType(types: !2100)
!2100 = !{!1678, !1773, !1773}
!2101 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2102, file: !1966, line: 182)
!2102 = !DISubprogram(name: "wcsftime", scope: !1972, file: !1972, line: 835, type: !2103, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2103 = !DISubroutineType(types: !2104)
!2104 = !{!1678, !1725, !1678, !1772, !2105}
!2105 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2106)
!2106 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2107, size: 64)
!2107 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2108)
!2108 = !DICompositeType(tag: DW_TAG_structure_type, name: "tm", file: !1972, line: 83, flags: DIFlagFwdDecl, identifier: "_ZTS2tm")
!2109 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2110, file: !1966, line: 183)
!2110 = !DISubprogram(name: "wcslen", scope: !1972, file: !1972, line: 222, type: !2111, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2111 = !DISubroutineType(types: !2112)
!2112 = !{!1678, !1773}
!2113 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2114, file: !1966, line: 184)
!2114 = !DISubprogram(name: "wcsncat", scope: !1986, file: !1986, line: 263, type: !2115, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2115 = !DISubroutineType(types: !2116)
!2116 = !{!1726, !1725, !1772, !1678}
!2117 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2118, file: !1966, line: 185)
!2118 = !DISubprogram(name: "wcsncmp", scope: !1972, file: !1972, line: 109, type: !2119, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2119 = !DISubroutineType(types: !2120)
!2120 = !{!93, !1773, !1773, !1678}
!2121 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2122, file: !1966, line: 186)
!2122 = !DISubprogram(name: "wcsncpy", scope: !1986, file: !1986, line: 191, type: !2115, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2123 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2124, file: !1966, line: 187)
!2124 = !DISubprogram(name: "wcsrtombs", scope: !1986, file: !1986, line: 508, type: !2125, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2125 = !DISubroutineType(types: !2126)
!2126 = !{!1678, !1771, !2127, !1678, !2018}
!2127 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2128)
!2128 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1773, size: 64)
!2129 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2130, file: !1966, line: 188)
!2130 = !DISubprogram(name: "wcsspn", scope: !1972, file: !1972, line: 191, type: !2099, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2131 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2132, file: !1966, line: 189)
!2132 = !DISubprogram(name: "wcstod", scope: !1972, file: !1972, line: 377, type: !2133, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2133 = !DISubroutineType(types: !2134)
!2134 = !{!1665, !1772, !2135}
!2135 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2136)
!2136 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1726, size: 64)
!2137 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2138, file: !1966, line: 191)
!2138 = !DISubprogram(name: "wcstof", scope: !1972, file: !1972, line: 382, type: !2139, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2139 = !DISubroutineType(types: !2140)
!2140 = !{!1813, !1772, !2135}
!2141 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2142, file: !1966, line: 193)
!2142 = !DISubprogram(name: "wcstok", scope: !1972, file: !1972, line: 217, type: !2143, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2143 = !DISubroutineType(types: !2144)
!2144 = !{!1726, !1725, !1772, !2135}
!2145 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2146, file: !1966, line: 194)
!2146 = !DISubprogram(name: "wcstol", scope: !1972, file: !1972, line: 428, type: !2147, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2147 = !DISubroutineType(types: !2148)
!2148 = !{!1504, !1772, !2135, !93}
!2149 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2150, file: !1966, line: 195)
!2150 = !DISubprogram(name: "wcstoul", scope: !1972, file: !1972, line: 433, type: !2151, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2151 = !DISubroutineType(types: !2152)
!2152 = !{!177, !1772, !2135, !93}
!2153 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2154, file: !1966, line: 196)
!2154 = !DISubprogram(name: "wcsxfrm", scope: !1972, file: !1972, line: 135, type: !2155, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2155 = !DISubroutineType(types: !2156)
!2156 = !{!1678, !1725, !1772, !1678}
!2157 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2158, file: !1966, line: 197)
!2158 = !DISubprogram(name: "wctob", scope: !1972, file: !1972, line: 288, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2159 = !DISubroutineType(types: !2160)
!2160 = !{!93, !1968}
!2161 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2162, file: !1966, line: 198)
!2162 = !DISubprogram(name: "wmemcmp", scope: !1972, file: !1972, line: 258, type: !2119, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2163 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2164, file: !1966, line: 199)
!2164 = !DISubprogram(name: "wmemcpy", scope: !1986, file: !1986, line: 39, type: !2115, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2165 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2166, file: !1966, line: 200)
!2166 = !DISubprogram(name: "wmemmove", scope: !1986, file: !1986, line: 68, type: !2167, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2167 = !DISubroutineType(types: !2168)
!2168 = !{!1726, !1726, !1773, !1678}
!2169 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2170, file: !1966, line: 201)
!2170 = !DISubprogram(name: "wmemset", scope: !1986, file: !1986, line: 129, type: !2171, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2171 = !DISubroutineType(types: !2172)
!2172 = !{!1726, !1726, !1727, !1678}
!2173 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2174, file: !1966, line: 202)
!2174 = !DISubprogram(name: "wprintf", scope: !1972, file: !1972, line: 587, type: !2175, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2175 = !DISubroutineType(types: !2176)
!2176 = !{!93, !1772, null}
!2177 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2178, file: !1966, line: 203)
!2178 = !DISubprogram(name: "wscanf", scope: !1972, file: !1972, line: 628, type: !2175, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2179 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2180, file: !1966, line: 204)
!2180 = !DISubprogram(name: "wcschr", scope: !1972, file: !1972, line: 164, type: !2181, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2181 = !DISubroutineType(types: !2182)
!2182 = !{!1726, !1773, !1727}
!2183 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2184, file: !1966, line: 205)
!2184 = !DISubprogram(name: "wcspbrk", scope: !1972, file: !1972, line: 201, type: !2185, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2185 = !DISubroutineType(types: !2186)
!2186 = !{!1726, !1773, !1773}
!2187 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2188, file: !1966, line: 206)
!2188 = !DISubprogram(name: "wcsrchr", scope: !1972, file: !1972, line: 174, type: !2181, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2189 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2190, file: !1966, line: 207)
!2190 = !DISubprogram(name: "wcsstr", scope: !1972, file: !1972, line: 212, type: !2185, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2191 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2192, file: !1966, line: 208)
!2192 = !DISubprogram(name: "wmemchr", scope: !1972, file: !1972, line: 253, type: !2193, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2193 = !DISubroutineType(types: !2194)
!2194 = !{!1726, !1773, !1727, !1678}
!2195 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2196, file: !1966, line: 248)
!2196 = !DISubprogram(name: "wcstold", scope: !1972, file: !1972, line: 384, type: !2197, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2197 = !DISubroutineType(types: !2198)
!2198 = !{!1818, !1772, !2135}
!2199 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2200, file: !1966, line: 257)
!2200 = !DISubprogram(name: "wcstoll", scope: !1972, file: !1972, line: 441, type: !2201, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2201 = !DISubroutineType(types: !2202)
!2202 = !{!1784, !1772, !2135, !93}
!2203 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2204, file: !1966, line: 258)
!2204 = !DISubprogram(name: "wcstoull", scope: !1972, file: !1972, line: 448, type: !2205, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2205 = !DISubroutineType(types: !2206)
!2206 = !{!1808, !1772, !2135, !93}
!2207 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2196, file: !1966, line: 264)
!2208 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2200, file: !1966, line: 265)
!2209 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2204, file: !1966, line: 266)
!2210 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2138, file: !1966, line: 280)
!2211 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2066, file: !1966, line: 283)
!2212 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2072, file: !1966, line: 286)
!2213 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2080, file: !1966, line: 289)
!2214 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2196, file: !1966, line: 293)
!2215 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2200, file: !1966, line: 294)
!2216 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2204, file: !1966, line: 295)
!2217 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2218, file: !2222, line: 48)
!2218 = !DIDerivedType(tag: DW_TAG_typedef, name: "int8_t", file: !2219, line: 24, baseType: !2220)
!2219 = !DIFile(filename: "/usr/include/bits/stdint-intn.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2220 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int8_t", file: !46, line: 36, baseType: !2221)
!2221 = !DIBasicType(name: "signed char", size: 8, encoding: DW_ATE_signed_char)
!2222 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cstdint", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2223 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2224, file: !2222, line: 49)
!2224 = !DIDerivedType(tag: DW_TAG_typedef, name: "int16_t", file: !2219, line: 25, baseType: !2225)
!2225 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int16_t", file: !46, line: 38, baseType: !2226)
!2226 = !DIBasicType(name: "short", size: 16, encoding: DW_ATE_signed)
!2227 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2228, file: !2222, line: 50)
!2228 = !DIDerivedType(tag: DW_TAG_typedef, name: "int32_t", file: !2219, line: 26, baseType: !2229)
!2229 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int32_t", file: !46, line: 40, baseType: !93)
!2230 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2231, file: !2222, line: 51)
!2231 = !DIDerivedType(tag: DW_TAG_typedef, name: "int64_t", file: !2219, line: 27, baseType: !2232)
!2232 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int64_t", file: !46, line: 43, baseType: !1504)
!2233 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2234, file: !2222, line: 53)
!2234 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast8_t", file: !2235, line: 58, baseType: !2221)
!2235 = !DIFile(filename: "/usr/include/stdint.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2236 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2237, file: !2222, line: 54)
!2237 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast16_t", file: !2235, line: 60, baseType: !1504)
!2238 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2239, file: !2222, line: 55)
!2239 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast32_t", file: !2235, line: 61, baseType: !1504)
!2240 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2241, file: !2222, line: 56)
!2241 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_fast64_t", file: !2235, line: 62, baseType: !1504)
!2242 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2243, file: !2222, line: 58)
!2243 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least8_t", file: !2235, line: 43, baseType: !2244)
!2244 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least8_t", file: !46, line: 51, baseType: !2220)
!2245 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2246, file: !2222, line: 59)
!2246 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least16_t", file: !2235, line: 44, baseType: !2247)
!2247 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least16_t", file: !46, line: 53, baseType: !2225)
!2248 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2249, file: !2222, line: 60)
!2249 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least32_t", file: !2235, line: 45, baseType: !2250)
!2250 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least32_t", file: !46, line: 55, baseType: !2229)
!2251 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2252, file: !2222, line: 61)
!2252 = !DIDerivedType(tag: DW_TAG_typedef, name: "int_least64_t", file: !2235, line: 46, baseType: !2253)
!2253 = !DIDerivedType(tag: DW_TAG_typedef, name: "__int_least64_t", file: !46, line: 57, baseType: !2232)
!2254 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2255, file: !2222, line: 63)
!2255 = !DIDerivedType(tag: DW_TAG_typedef, name: "intmax_t", file: !2235, line: 101, baseType: !2256)
!2256 = !DIDerivedType(tag: DW_TAG_typedef, name: "__intmax_t", file: !46, line: 71, baseType: !1504)
!2257 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2258, file: !2222, line: 64)
!2258 = !DIDerivedType(tag: DW_TAG_typedef, name: "intptr_t", file: !2235, line: 87, baseType: !1504)
!2259 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2260, file: !2222, line: 66)
!2260 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint8_t", file: !44, line: 24, baseType: !2261)
!2261 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint8_t", file: !46, line: 37, baseType: !491)
!2262 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2263, file: !2222, line: 67)
!2263 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint16_t", file: !44, line: 25, baseType: !2264)
!2264 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint16_t", file: !46, line: 39, baseType: !2265)
!2265 = !DIBasicType(name: "unsigned short", size: 16, encoding: DW_ATE_unsigned)
!2266 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !43, file: !2222, line: 68)
!2267 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2268, file: !2222, line: 69)
!2268 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint64_t", file: !44, line: 27, baseType: !2269)
!2269 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint64_t", file: !46, line: 44, baseType: !177)
!2270 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2271, file: !2222, line: 71)
!2271 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast8_t", file: !2235, line: 71, baseType: !491)
!2272 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2273, file: !2222, line: 72)
!2273 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast16_t", file: !2235, line: 73, baseType: !177)
!2274 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2275, file: !2222, line: 73)
!2275 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast32_t", file: !2235, line: 74, baseType: !177)
!2276 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2277, file: !2222, line: 74)
!2277 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_fast64_t", file: !2235, line: 75, baseType: !177)
!2278 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2279, file: !2222, line: 76)
!2279 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least8_t", file: !2235, line: 49, baseType: !2280)
!2280 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least8_t", file: !46, line: 52, baseType: !2261)
!2281 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2282, file: !2222, line: 77)
!2282 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least16_t", file: !2235, line: 50, baseType: !2283)
!2283 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least16_t", file: !46, line: 54, baseType: !2264)
!2284 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2285, file: !2222, line: 78)
!2285 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least32_t", file: !2235, line: 51, baseType: !2286)
!2286 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least32_t", file: !46, line: 56, baseType: !45)
!2287 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2288, file: !2222, line: 79)
!2288 = !DIDerivedType(tag: DW_TAG_typedef, name: "uint_least64_t", file: !2235, line: 52, baseType: !2289)
!2289 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uint_least64_t", file: !46, line: 58, baseType: !2269)
!2290 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2291, file: !2222, line: 81)
!2291 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintmax_t", file: !2235, line: 102, baseType: !2292)
!2292 = !DIDerivedType(tag: DW_TAG_typedef, name: "__uintmax_t", file: !46, line: 72, baseType: !177)
!2293 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2294, file: !2222, line: 82)
!2294 = !DIDerivedType(tag: DW_TAG_typedef, name: "uintptr_t", file: !2235, line: 90, baseType: !177)
!2295 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !175, file: !147, line: 44)
!2296 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !1503, file: !147, line: 45)
!2297 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2298, file: !2300, line: 53)
!2298 = !DICompositeType(tag: DW_TAG_structure_type, name: "lconv", file: !2299, line: 51, flags: DIFlagFwdDecl, identifier: "_ZTS5lconv")
!2299 = !DIFile(filename: "/usr/include/locale.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2300 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/clocale", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2301 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2302, file: !2300, line: 54)
!2302 = !DISubprogram(name: "setlocale", scope: !2299, file: !2299, line: 122, type: !2303, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2303 = !DISubroutineType(types: !2304)
!2304 = !{!1170, !93, !1443}
!2305 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2306, file: !2300, line: 55)
!2306 = !DISubprogram(name: "localeconv", scope: !2299, file: !2299, line: 125, type: !2307, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2307 = !DISubroutineType(types: !2308)
!2308 = !{!2309}
!2309 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2298, size: 64)
!2310 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2311, file: !2313, line: 64)
!2311 = !DISubprogram(name: "isalnum", scope: !2312, file: !2312, line: 108, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2312 = !DIFile(filename: "/usr/include/ctype.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2313 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cctype", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2314 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2315, file: !2313, line: 65)
!2315 = !DISubprogram(name: "isalpha", scope: !2312, file: !2312, line: 109, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2316 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2317, file: !2313, line: 66)
!2317 = !DISubprogram(name: "iscntrl", scope: !2312, file: !2312, line: 110, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2318 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2319, file: !2313, line: 67)
!2319 = !DISubprogram(name: "isdigit", scope: !2312, file: !2312, line: 111, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2320 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2321, file: !2313, line: 68)
!2321 = !DISubprogram(name: "isgraph", scope: !2312, file: !2312, line: 113, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2322 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2323, file: !2313, line: 69)
!2323 = !DISubprogram(name: "islower", scope: !2312, file: !2312, line: 112, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2324 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2325, file: !2313, line: 70)
!2325 = !DISubprogram(name: "isprint", scope: !2312, file: !2312, line: 114, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2326 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2327, file: !2313, line: 71)
!2327 = !DISubprogram(name: "ispunct", scope: !2312, file: !2312, line: 115, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2328 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2329, file: !2313, line: 72)
!2329 = !DISubprogram(name: "isspace", scope: !2312, file: !2312, line: 116, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2330 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2331, file: !2313, line: 73)
!2331 = !DISubprogram(name: "isupper", scope: !2312, file: !2312, line: 117, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2332 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2333, file: !2313, line: 74)
!2333 = !DISubprogram(name: "isxdigit", scope: !2312, file: !2312, line: 118, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2334 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2335, file: !2313, line: 75)
!2335 = !DISubprogram(name: "tolower", scope: !2312, file: !2312, line: 122, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2336 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2337, file: !2313, line: 76)
!2337 = !DISubprogram(name: "toupper", scope: !2312, file: !2312, line: 125, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2338 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2339, file: !2313, line: 87)
!2339 = !DISubprogram(name: "isblank", scope: !2312, file: !2312, line: 130, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2340 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2341, file: !2343, line: 98)
!2341 = !DIDerivedType(tag: DW_TAG_typedef, name: "FILE", file: !2342, line: 7, baseType: !1982)
!2342 = !DIFile(filename: "/usr/include/bits/types/FILE.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2343 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cstdio", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2344 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2345, file: !2343, line: 99)
!2345 = !DIDerivedType(tag: DW_TAG_typedef, name: "fpos_t", file: !2346, line: 84, baseType: !2347)
!2346 = !DIFile(filename: "/usr/include/stdio.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2347 = !DIDerivedType(tag: DW_TAG_typedef, name: "__fpos_t", file: !2348, line: 14, baseType: !2349)
!2348 = !DIFile(filename: "/usr/include/bits/types/__fpos_t.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2349 = !DICompositeType(tag: DW_TAG_structure_type, name: "_G_fpos_t", file: !2348, line: 10, flags: DIFlagFwdDecl, identifier: "_ZTS9_G_fpos_t")
!2350 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2351, file: !2343, line: 101)
!2351 = !DISubprogram(name: "clearerr", scope: !2346, file: !2346, line: 763, type: !2352, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2352 = !DISubroutineType(types: !2353)
!2353 = !{null, !2354}
!2354 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2341, size: 64)
!2355 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2356, file: !2343, line: 102)
!2356 = !DISubprogram(name: "fclose", scope: !2346, file: !2346, line: 213, type: !2357, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2357 = !DISubroutineType(types: !2358)
!2358 = !{!93, !2354}
!2359 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2360, file: !2343, line: 103)
!2360 = !DISubprogram(name: "feof", scope: !2346, file: !2346, line: 765, type: !2357, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2361 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2362, file: !2343, line: 104)
!2362 = !DISubprogram(name: "ferror", scope: !2346, file: !2346, line: 767, type: !2357, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2363 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2364, file: !2343, line: 105)
!2364 = !DISubprogram(name: "fflush", scope: !2346, file: !2346, line: 218, type: !2357, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2365 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2366, file: !2343, line: 106)
!2366 = !DISubprogram(name: "fgetc", scope: !2346, file: !2346, line: 491, type: !2357, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2367 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2368, file: !2343, line: 107)
!2368 = !DISubprogram(name: "fgetpos", scope: !2346, file: !2346, line: 737, type: !2369, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2369 = !DISubroutineType(types: !2370)
!2370 = !{!93, !2371, !2372}
!2371 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2354)
!2372 = !DIDerivedType(tag: DW_TAG_restrict_type, baseType: !2373)
!2373 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2345, size: 64)
!2374 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2375, file: !2343, line: 108)
!2375 = !DISubprogram(name: "fgets", scope: !2376, file: !2376, line: 255, type: !2377, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2376 = !DIFile(filename: "/usr/include/bits/stdio2.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2377 = !DISubroutineType(types: !2378)
!2378 = !{!1170, !1771, !93, !2371}
!2379 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2380, file: !2343, line: 109)
!2380 = !DISubprogram(name: "fopen", scope: !2346, file: !2346, line: 246, type: !2381, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2381 = !DISubroutineType(types: !2382)
!2382 = !{!2354, !1728, !1728}
!2383 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2384, file: !2343, line: 110)
!2384 = !DISubprogram(name: "fprintf", scope: !2346, file: !2346, line: 326, type: !2385, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2385 = !DISubroutineType(types: !2386)
!2386 = !{!93, !2371, !1728, null}
!2387 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2388, file: !2343, line: 111)
!2388 = !DISubprogram(name: "fputc", scope: !2346, file: !2346, line: 527, type: !2389, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2389 = !DISubroutineType(types: !2390)
!2390 = !{!93, !93, !2354}
!2391 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2392, file: !2343, line: 112)
!2392 = !DISubprogram(name: "fputs", scope: !2346, file: !2346, line: 632, type: !2393, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2393 = !DISubroutineType(types: !2394)
!2394 = !{!93, !1728, !2371}
!2395 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2396, file: !2343, line: 113)
!2396 = !DISubprogram(name: "fread", scope: !2376, file: !2376, line: 284, type: !2397, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2397 = !DISubroutineType(types: !2398)
!2398 = !{!1678, !1882, !1678, !1678, !2371}
!2399 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2400, file: !2343, line: 114)
!2400 = !DISubprogram(name: "freopen", scope: !2346, file: !2346, line: 252, type: !2401, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2401 = !DISubroutineType(types: !2402)
!2402 = !{!2354, !1728, !1728, !2371}
!2403 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2404, file: !2343, line: 115)
!2404 = !DISubprogram(name: "fscanf", scope: !2346, file: !2346, line: 391, type: !2385, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2405 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2406, file: !2343, line: 116)
!2406 = !DISubprogram(name: "fseek", scope: !2346, file: !2346, line: 690, type: !2407, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2407 = !DISubroutineType(types: !2408)
!2408 = !{!93, !2354, !1504, !93}
!2409 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2410, file: !2343, line: 117)
!2410 = !DISubprogram(name: "fsetpos", scope: !2346, file: !2346, line: 742, type: !2411, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2411 = !DISubroutineType(types: !2412)
!2412 = !{!93, !2354, !2413}
!2413 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2414, size: 64)
!2414 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2345)
!2415 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2416, file: !2343, line: 118)
!2416 = !DISubprogram(name: "ftell", scope: !2346, file: !2346, line: 695, type: !2417, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2417 = !DISubroutineType(types: !2418)
!2418 = !{!1504, !2354}
!2419 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2420, file: !2343, line: 119)
!2420 = !DISubprogram(name: "fwrite", scope: !2346, file: !2346, line: 658, type: !2421, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2421 = !DISubroutineType(types: !2422)
!2422 = !{!1678, !1883, !1678, !1678, !2371}
!2423 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2424, file: !2343, line: 120)
!2424 = !DISubprogram(name: "getc", scope: !2346, file: !2346, line: 492, type: !2357, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2425 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2426, file: !2343, line: 121)
!2426 = !DISubprogram(name: "getchar", scope: !2346, file: !2346, line: 498, type: !1741, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2427 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2428, file: !2343, line: 126)
!2428 = !DISubprogram(name: "perror", scope: !2346, file: !2346, line: 781, type: !2429, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2429 = !DISubroutineType(types: !2430)
!2430 = !{null, !1443}
!2431 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2432, file: !2343, line: 127)
!2432 = !DISubprogram(name: "printf", scope: !2346, file: !2346, line: 332, type: !2433, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2433 = !DISubroutineType(types: !2434)
!2434 = !{!93, !1728, null}
!2435 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2436, file: !2343, line: 128)
!2436 = !DISubprogram(name: "putc", scope: !2346, file: !2346, line: 528, type: !2389, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2437 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2438, file: !2343, line: 129)
!2438 = !DISubprogram(name: "putchar", scope: !2346, file: !2346, line: 534, type: !1637, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2439 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2440, file: !2343, line: 130)
!2440 = !DISubprogram(name: "puts", scope: !2346, file: !2346, line: 638, type: !1668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2441 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2442, file: !2343, line: 131)
!2442 = !DISubprogram(name: "remove", scope: !2346, file: !2346, line: 146, type: !1668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2443 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2444, file: !2343, line: 132)
!2444 = !DISubprogram(name: "rename", scope: !2346, file: !2346, line: 148, type: !1898, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2445 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2446, file: !2343, line: 133)
!2446 = !DISubprogram(name: "rewind", scope: !2346, file: !2346, line: 700, type: !2352, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2447 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2448, file: !2343, line: 134)
!2448 = !DISubprogram(name: "scanf", scope: !2346, file: !2346, line: 397, type: !2433, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2449 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2450, file: !2343, line: 135)
!2450 = !DISubprogram(name: "setbuf", scope: !2346, file: !2346, line: 304, type: !2451, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2451 = !DISubroutineType(types: !2452)
!2452 = !{null, !2371, !1771}
!2453 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2454, file: !2343, line: 136)
!2454 = !DISubprogram(name: "setvbuf", scope: !2346, file: !2346, line: 308, type: !2455, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2455 = !DISubroutineType(types: !2456)
!2456 = !{!93, !2371, !1771, !93, !1678}
!2457 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2458, file: !2343, line: 137)
!2458 = !DISubprogram(name: "sprintf", scope: !2346, file: !2346, line: 334, type: !2459, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2459 = !DISubroutineType(types: !2460)
!2460 = !{!93, !1771, !1728, null}
!2461 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2462, file: !2343, line: 138)
!2462 = !DISubprogram(name: "sscanf", scope: !2346, file: !2346, line: 399, type: !2463, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2463 = !DISubroutineType(types: !2464)
!2464 = !{!93, !1728, !1728, null}
!2465 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2466, file: !2343, line: 139)
!2466 = !DISubprogram(name: "tmpfile", scope: !2346, file: !2346, line: 173, type: !2467, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2467 = !DISubroutineType(types: !2468)
!2468 = !{!2354}
!2469 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2470, file: !2343, line: 141)
!2470 = !DISubprogram(name: "tmpnam", scope: !2346, file: !2346, line: 187, type: !2471, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2471 = !DISubroutineType(types: !2472)
!2472 = !{!1170, !1170}
!2473 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2474, file: !2343, line: 143)
!2474 = !DISubprogram(name: "ungetc", scope: !2346, file: !2346, line: 645, type: !2389, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2475 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2476, file: !2343, line: 144)
!2476 = !DISubprogram(name: "vfprintf", scope: !2376, file: !2376, line: 127, type: !2477, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2477 = !DISubroutineType(types: !2478)
!2478 = !{!93, !2371, !1728, !2058}
!2479 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2480, file: !2343, line: 145)
!2480 = !DISubprogram(name: "vprintf", scope: !2376, file: !2376, line: 117, type: !2481, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2481 = !DISubroutineType(types: !2482)
!2482 = !{!93, !1728, !2058}
!2483 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2484, file: !2343, line: 146)
!2484 = !DISubprogram(name: "vsprintf", scope: !2376, file: !2376, line: 46, type: !2485, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2485 = !DISubroutineType(types: !2486)
!2486 = !{!93, !1771, !1728, !2058}
!2487 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2488, file: !2343, line: 175)
!2488 = !DISubprogram(name: "snprintf", scope: !2346, file: !2346, line: 354, type: !2489, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2489 = !DISubroutineType(types: !2490)
!2490 = !{!93, !1771, !1678, !1728, null}
!2491 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2492, file: !2343, line: 176)
!2492 = !DISubprogram(name: "vfscanf", scope: !2346, file: !2346, line: 434, type: !2477, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2493 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2494, file: !2343, line: 177)
!2494 = !DISubprogram(name: "vscanf", scope: !2346, file: !2346, line: 442, type: !2481, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2495 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2496, file: !2343, line: 178)
!2496 = !DISubprogram(name: "vsnprintf", scope: !2376, file: !2376, line: 77, type: !2497, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2497 = !DISubroutineType(types: !2498)
!2498 = !{!93, !1771, !1678, !1728, !2058}
!2499 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !5, entity: !2500, file: !2343, line: 179)
!2500 = !DISubprogram(name: "vsscanf", scope: !2346, file: !2346, line: 446, type: !2501, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2501 = !DISubroutineType(types: !2502)
!2502 = !{!93, !1728, !1728, !2058}
!2503 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2488, file: !2343, line: 185)
!2504 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2492, file: !2343, line: 186)
!2505 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2494, file: !2343, line: 187)
!2506 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2496, file: !2343, line: 188)
!2507 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2500, file: !2343, line: 189)
!2508 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2509, file: !2513, line: 82)
!2509 = !DIDerivedType(tag: DW_TAG_typedef, name: "wctrans_t", file: !2510, line: 48, baseType: !2511)
!2510 = !DIFile(filename: "/usr/include/wctype.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2511 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2512, size: 64)
!2512 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2229)
!2513 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cwctype", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2514 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2515, file: !2513, line: 83)
!2515 = !DIDerivedType(tag: DW_TAG_typedef, name: "wctype_t", file: !2516, line: 38, baseType: !177)
!2516 = !DIFile(filename: "/usr/include/bits/wctype-wchar.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2517 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !1968, file: !2513, line: 84)
!2518 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2519, file: !2513, line: 86)
!2519 = !DISubprogram(name: "iswalnum", scope: !2516, file: !2516, line: 95, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2520 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2521, file: !2513, line: 87)
!2521 = !DISubprogram(name: "iswalpha", scope: !2516, file: !2516, line: 101, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2522 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2523, file: !2513, line: 89)
!2523 = !DISubprogram(name: "iswblank", scope: !2516, file: !2516, line: 146, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2524 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2525, file: !2513, line: 91)
!2525 = !DISubprogram(name: "iswcntrl", scope: !2516, file: !2516, line: 104, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2526 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2527, file: !2513, line: 92)
!2527 = !DISubprogram(name: "iswctype", scope: !2516, file: !2516, line: 159, type: !2528, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2528 = !DISubroutineType(types: !2529)
!2529 = !{!93, !1968, !2515}
!2530 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2531, file: !2513, line: 93)
!2531 = !DISubprogram(name: "iswdigit", scope: !2516, file: !2516, line: 108, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2532 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2533, file: !2513, line: 94)
!2533 = !DISubprogram(name: "iswgraph", scope: !2516, file: !2516, line: 112, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2534 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2535, file: !2513, line: 95)
!2535 = !DISubprogram(name: "iswlower", scope: !2516, file: !2516, line: 117, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2536 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2537, file: !2513, line: 96)
!2537 = !DISubprogram(name: "iswprint", scope: !2516, file: !2516, line: 120, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2538 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2539, file: !2513, line: 97)
!2539 = !DISubprogram(name: "iswpunct", scope: !2516, file: !2516, line: 125, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2540 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2541, file: !2513, line: 98)
!2541 = !DISubprogram(name: "iswspace", scope: !2516, file: !2516, line: 130, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2542 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2543, file: !2513, line: 99)
!2543 = !DISubprogram(name: "iswupper", scope: !2516, file: !2516, line: 135, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2544 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2545, file: !2513, line: 100)
!2545 = !DISubprogram(name: "iswxdigit", scope: !2516, file: !2516, line: 140, type: !2159, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2546 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2547, file: !2513, line: 101)
!2547 = !DISubprogram(name: "towctrans", scope: !2510, file: !2510, line: 55, type: !2548, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2548 = !DISubroutineType(types: !2549)
!2549 = !{!1968, !1968, !2509}
!2550 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2551, file: !2513, line: 102)
!2551 = !DISubprogram(name: "towlower", scope: !2516, file: !2516, line: 166, type: !2552, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2552 = !DISubroutineType(types: !2553)
!2553 = !{!1968, !1968}
!2554 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2555, file: !2513, line: 103)
!2555 = !DISubprogram(name: "towupper", scope: !2516, file: !2516, line: 169, type: !2552, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2556 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2557, file: !2513, line: 104)
!2557 = !DISubprogram(name: "wctrans", scope: !2510, file: !2510, line: 52, type: !2558, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2558 = !DISubroutineType(types: !2559)
!2559 = !{!2509, !1443}
!2560 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2561, file: !2513, line: 105)
!2561 = !DISubprogram(name: "wctype", scope: !2516, file: !2516, line: 155, type: !2562, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2562 = !DISubroutineType(types: !2563)
!2563 = !{!2515, !1443}
!2564 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !3, file: !2565, line: 86)
!2565 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/shared_ptr_base.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2566 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2567, file: !2565, line: 87)
!2567 = !DIGlobalVariable(name: "__default_lock_policy", linkageName: "_ZN9__gnu_cxxL21__default_lock_policyE", scope: !5, file: !4, line: 53, type: !2568, isLocal: true, isDefinition: false)
!2568 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3)
!2569 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2570, file: !2574, line: 83)
!2570 = !DISubprogram(name: "acos", scope: !2571, file: !2571, line: 53, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2571 = !DIFile(filename: "/usr/include/bits/mathcalls.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2572 = !DISubroutineType(types: !2573)
!2573 = !{!1665, !1665}
!2574 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/cmath", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2575 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2576, file: !2574, line: 102)
!2576 = !DISubprogram(name: "asin", scope: !2571, file: !2571, line: 55, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2577 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2578, file: !2574, line: 121)
!2578 = !DISubprogram(name: "atan", scope: !2571, file: !2571, line: 57, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2579 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2580, file: !2574, line: 140)
!2580 = !DISubprogram(name: "atan2", scope: !2571, file: !2571, line: 59, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2581 = !DISubroutineType(types: !2582)
!2582 = !{!1665, !1665, !1665}
!2583 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2584, file: !2574, line: 161)
!2584 = !DISubprogram(name: "ceil", scope: !2571, file: !2571, line: 159, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2585 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2586, file: !2574, line: 180)
!2586 = !DISubprogram(name: "cos", scope: !2571, file: !2571, line: 62, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2587 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2588, file: !2574, line: 199)
!2588 = !DISubprogram(name: "cosh", scope: !2571, file: !2571, line: 71, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2589 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2590, file: !2574, line: 218)
!2590 = !DISubprogram(name: "exp", scope: !2571, file: !2571, line: 95, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2591 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2592, file: !2574, line: 237)
!2592 = !DISubprogram(name: "fabs", scope: !2571, file: !2571, line: 162, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2593 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2594, file: !2574, line: 256)
!2594 = !DISubprogram(name: "floor", scope: !2571, file: !2571, line: 165, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2595 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2596, file: !2574, line: 275)
!2596 = !DISubprogram(name: "fmod", scope: !2571, file: !2571, line: 168, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2597 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2598, file: !2574, line: 296)
!2598 = !DISubprogram(name: "frexp", scope: !2571, file: !2571, line: 98, type: !2599, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2599 = !DISubroutineType(types: !2600)
!2600 = !{!1665, !1665, !2601}
!2601 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !93, size: 64)
!2602 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2603, file: !2574, line: 315)
!2603 = !DISubprogram(name: "ldexp", scope: !2571, file: !2571, line: 101, type: !2604, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2604 = !DISubroutineType(types: !2605)
!2605 = !{!1665, !1665, !93}
!2606 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2607, file: !2574, line: 334)
!2607 = !DISubprogram(name: "log", scope: !2571, file: !2571, line: 104, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2608 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2609, file: !2574, line: 353)
!2609 = !DISubprogram(name: "log10", scope: !2571, file: !2571, line: 107, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2610 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2611, file: !2574, line: 372)
!2611 = !DISubprogram(name: "modf", scope: !2571, file: !2571, line: 110, type: !2612, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2612 = !DISubroutineType(types: !2613)
!2613 = !{!1665, !1665, !2614}
!2614 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1665, size: 64)
!2615 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2616, file: !2574, line: 384)
!2616 = !DISubprogram(name: "pow", scope: !2571, file: !2571, line: 140, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2617 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2618, file: !2574, line: 421)
!2618 = !DISubprogram(name: "sin", scope: !2571, file: !2571, line: 64, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2619 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2620, file: !2574, line: 440)
!2620 = !DISubprogram(name: "sinh", scope: !2571, file: !2571, line: 73, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2621 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2622, file: !2574, line: 459)
!2622 = !DISubprogram(name: "sqrt", scope: !2571, file: !2571, line: 143, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2623 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2624, file: !2574, line: 478)
!2624 = !DISubprogram(name: "tan", scope: !2571, file: !2571, line: 66, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2625 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2626, file: !2574, line: 497)
!2626 = !DISubprogram(name: "tanh", scope: !2571, file: !2571, line: 75, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2627 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2628, file: !2574, line: 1065)
!2628 = !DIDerivedType(tag: DW_TAG_typedef, name: "double_t", file: !2629, line: 150, baseType: !1665)
!2629 = !DIFile(filename: "/usr/include/math.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2630 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2631, file: !2574, line: 1066)
!2631 = !DIDerivedType(tag: DW_TAG_typedef, name: "float_t", file: !2629, line: 149, baseType: !1813)
!2632 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2633, file: !2574, line: 1069)
!2633 = !DISubprogram(name: "acosh", scope: !2571, file: !2571, line: 85, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2634 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2635, file: !2574, line: 1070)
!2635 = !DISubprogram(name: "acoshf", scope: !2571, file: !2571, line: 85, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2636 = !DISubroutineType(types: !2637)
!2637 = !{!1813, !1813}
!2638 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2639, file: !2574, line: 1071)
!2639 = !DISubprogram(name: "acoshl", scope: !2571, file: !2571, line: 85, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2640 = !DISubroutineType(types: !2641)
!2641 = !{!1818, !1818}
!2642 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2643, file: !2574, line: 1073)
!2643 = !DISubprogram(name: "asinh", scope: !2571, file: !2571, line: 87, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2644 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2645, file: !2574, line: 1074)
!2645 = !DISubprogram(name: "asinhf", scope: !2571, file: !2571, line: 87, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2646 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2647, file: !2574, line: 1075)
!2647 = !DISubprogram(name: "asinhl", scope: !2571, file: !2571, line: 87, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2648 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2649, file: !2574, line: 1077)
!2649 = !DISubprogram(name: "atanh", scope: !2571, file: !2571, line: 89, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2650 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2651, file: !2574, line: 1078)
!2651 = !DISubprogram(name: "atanhf", scope: !2571, file: !2571, line: 89, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2652 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2653, file: !2574, line: 1079)
!2653 = !DISubprogram(name: "atanhl", scope: !2571, file: !2571, line: 89, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2654 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2655, file: !2574, line: 1081)
!2655 = !DISubprogram(name: "cbrt", scope: !2571, file: !2571, line: 152, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2656 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2657, file: !2574, line: 1082)
!2657 = !DISubprogram(name: "cbrtf", scope: !2571, file: !2571, line: 152, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2658 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2659, file: !2574, line: 1083)
!2659 = !DISubprogram(name: "cbrtl", scope: !2571, file: !2571, line: 152, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2660 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2661, file: !2574, line: 1085)
!2661 = !DISubprogram(name: "copysign", scope: !2571, file: !2571, line: 196, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2662 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2663, file: !2574, line: 1086)
!2663 = !DISubprogram(name: "copysignf", scope: !2571, file: !2571, line: 196, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2664 = !DISubroutineType(types: !2665)
!2665 = !{!1813, !1813, !1813}
!2666 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2667, file: !2574, line: 1087)
!2667 = !DISubprogram(name: "copysignl", scope: !2571, file: !2571, line: 196, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2668 = !DISubroutineType(types: !2669)
!2669 = !{!1818, !1818, !1818}
!2670 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2671, file: !2574, line: 1089)
!2671 = !DISubprogram(name: "erf", scope: !2571, file: !2571, line: 228, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2672 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2673, file: !2574, line: 1090)
!2673 = !DISubprogram(name: "erff", scope: !2571, file: !2571, line: 228, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2674 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2675, file: !2574, line: 1091)
!2675 = !DISubprogram(name: "erfl", scope: !2571, file: !2571, line: 228, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2676 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2677, file: !2574, line: 1093)
!2677 = !DISubprogram(name: "erfc", scope: !2571, file: !2571, line: 229, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2678 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2679, file: !2574, line: 1094)
!2679 = !DISubprogram(name: "erfcf", scope: !2571, file: !2571, line: 229, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2680 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2681, file: !2574, line: 1095)
!2681 = !DISubprogram(name: "erfcl", scope: !2571, file: !2571, line: 229, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2682 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2683, file: !2574, line: 1097)
!2683 = !DISubprogram(name: "exp2", scope: !2571, file: !2571, line: 130, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2684 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2685, file: !2574, line: 1098)
!2685 = !DISubprogram(name: "exp2f", scope: !2571, file: !2571, line: 130, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2686 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2687, file: !2574, line: 1099)
!2687 = !DISubprogram(name: "exp2l", scope: !2571, file: !2571, line: 130, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2688 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2689, file: !2574, line: 1101)
!2689 = !DISubprogram(name: "expm1", scope: !2571, file: !2571, line: 119, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2690 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2691, file: !2574, line: 1102)
!2691 = !DISubprogram(name: "expm1f", scope: !2571, file: !2571, line: 119, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2692 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2693, file: !2574, line: 1103)
!2693 = !DISubprogram(name: "expm1l", scope: !2571, file: !2571, line: 119, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2694 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2695, file: !2574, line: 1105)
!2695 = !DISubprogram(name: "fdim", scope: !2571, file: !2571, line: 326, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2696 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2697, file: !2574, line: 1106)
!2697 = !DISubprogram(name: "fdimf", scope: !2571, file: !2571, line: 326, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2698 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2699, file: !2574, line: 1107)
!2699 = !DISubprogram(name: "fdiml", scope: !2571, file: !2571, line: 326, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2700 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2701, file: !2574, line: 1109)
!2701 = !DISubprogram(name: "fma", scope: !2571, file: !2571, line: 335, type: !2702, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2702 = !DISubroutineType(types: !2703)
!2703 = !{!1665, !1665, !1665, !1665}
!2704 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2705, file: !2574, line: 1110)
!2705 = !DISubprogram(name: "fmaf", scope: !2571, file: !2571, line: 335, type: !2706, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2706 = !DISubroutineType(types: !2707)
!2707 = !{!1813, !1813, !1813, !1813}
!2708 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2709, file: !2574, line: 1111)
!2709 = !DISubprogram(name: "fmal", scope: !2571, file: !2571, line: 335, type: !2710, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2710 = !DISubroutineType(types: !2711)
!2711 = !{!1818, !1818, !1818, !1818}
!2712 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2713, file: !2574, line: 1113)
!2713 = !DISubprogram(name: "fmax", scope: !2571, file: !2571, line: 329, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2714 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2715, file: !2574, line: 1114)
!2715 = !DISubprogram(name: "fmaxf", scope: !2571, file: !2571, line: 329, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2716 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2717, file: !2574, line: 1115)
!2717 = !DISubprogram(name: "fmaxl", scope: !2571, file: !2571, line: 329, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2718 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2719, file: !2574, line: 1117)
!2719 = !DISubprogram(name: "fmin", scope: !2571, file: !2571, line: 332, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2720 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2721, file: !2574, line: 1118)
!2721 = !DISubprogram(name: "fminf", scope: !2571, file: !2571, line: 332, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2722 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2723, file: !2574, line: 1119)
!2723 = !DISubprogram(name: "fminl", scope: !2571, file: !2571, line: 332, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2724 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2725, file: !2574, line: 1121)
!2725 = !DISubprogram(name: "hypot", scope: !2571, file: !2571, line: 147, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2726 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2727, file: !2574, line: 1122)
!2727 = !DISubprogram(name: "hypotf", scope: !2571, file: !2571, line: 147, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2728 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2729, file: !2574, line: 1123)
!2729 = !DISubprogram(name: "hypotl", scope: !2571, file: !2571, line: 147, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2730 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2731, file: !2574, line: 1125)
!2731 = !DISubprogram(name: "ilogb", scope: !2571, file: !2571, line: 280, type: !2732, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2732 = !DISubroutineType(types: !2733)
!2733 = !{!93, !1665}
!2734 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2735, file: !2574, line: 1126)
!2735 = !DISubprogram(name: "ilogbf", scope: !2571, file: !2571, line: 280, type: !2736, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2736 = !DISubroutineType(types: !2737)
!2737 = !{!93, !1813}
!2738 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2739, file: !2574, line: 1127)
!2739 = !DISubprogram(name: "ilogbl", scope: !2571, file: !2571, line: 280, type: !2740, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2740 = !DISubroutineType(types: !2741)
!2741 = !{!93, !1818}
!2742 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2743, file: !2574, line: 1129)
!2743 = !DISubprogram(name: "lgamma", scope: !2571, file: !2571, line: 230, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2744 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2745, file: !2574, line: 1130)
!2745 = !DISubprogram(name: "lgammaf", scope: !2571, file: !2571, line: 230, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2746 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2747, file: !2574, line: 1131)
!2747 = !DISubprogram(name: "lgammal", scope: !2571, file: !2571, line: 230, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2748 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2749, file: !2574, line: 1134)
!2749 = !DISubprogram(name: "llrint", scope: !2571, file: !2571, line: 316, type: !2750, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2750 = !DISubroutineType(types: !2751)
!2751 = !{!1784, !1665}
!2752 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2753, file: !2574, line: 1135)
!2753 = !DISubprogram(name: "llrintf", scope: !2571, file: !2571, line: 316, type: !2754, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2754 = !DISubroutineType(types: !2755)
!2755 = !{!1784, !1813}
!2756 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2757, file: !2574, line: 1136)
!2757 = !DISubprogram(name: "llrintl", scope: !2571, file: !2571, line: 316, type: !2758, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2758 = !DISubroutineType(types: !2759)
!2759 = !{!1784, !1818}
!2760 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2761, file: !2574, line: 1138)
!2761 = !DISubprogram(name: "llround", scope: !2571, file: !2571, line: 322, type: !2750, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2762 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2763, file: !2574, line: 1139)
!2763 = !DISubprogram(name: "llroundf", scope: !2571, file: !2571, line: 322, type: !2754, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2764 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2765, file: !2574, line: 1140)
!2765 = !DISubprogram(name: "llroundl", scope: !2571, file: !2571, line: 322, type: !2758, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2766 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2767, file: !2574, line: 1143)
!2767 = !DISubprogram(name: "log1p", scope: !2571, file: !2571, line: 122, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2768 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2769, file: !2574, line: 1144)
!2769 = !DISubprogram(name: "log1pf", scope: !2571, file: !2571, line: 122, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2770 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2771, file: !2574, line: 1145)
!2771 = !DISubprogram(name: "log1pl", scope: !2571, file: !2571, line: 122, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2772 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2773, file: !2574, line: 1147)
!2773 = !DISubprogram(name: "log2", scope: !2571, file: !2571, line: 133, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2774 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2775, file: !2574, line: 1148)
!2775 = !DISubprogram(name: "log2f", scope: !2571, file: !2571, line: 133, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2776 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2777, file: !2574, line: 1149)
!2777 = !DISubprogram(name: "log2l", scope: !2571, file: !2571, line: 133, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2778 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2779, file: !2574, line: 1151)
!2779 = !DISubprogram(name: "logb", scope: !2571, file: !2571, line: 125, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2780 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2781, file: !2574, line: 1152)
!2781 = !DISubprogram(name: "logbf", scope: !2571, file: !2571, line: 125, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2782 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2783, file: !2574, line: 1153)
!2783 = !DISubprogram(name: "logbl", scope: !2571, file: !2571, line: 125, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2784 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2785, file: !2574, line: 1155)
!2785 = !DISubprogram(name: "lrint", scope: !2571, file: !2571, line: 314, type: !2786, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2786 = !DISubroutineType(types: !2787)
!2787 = !{!1504, !1665}
!2788 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2789, file: !2574, line: 1156)
!2789 = !DISubprogram(name: "lrintf", scope: !2571, file: !2571, line: 314, type: !2790, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2790 = !DISubroutineType(types: !2791)
!2791 = !{!1504, !1813}
!2792 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2793, file: !2574, line: 1157)
!2793 = !DISubprogram(name: "lrintl", scope: !2571, file: !2571, line: 314, type: !2794, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2794 = !DISubroutineType(types: !2795)
!2795 = !{!1504, !1818}
!2796 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2797, file: !2574, line: 1159)
!2797 = !DISubprogram(name: "lround", scope: !2571, file: !2571, line: 320, type: !2786, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2798 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2799, file: !2574, line: 1160)
!2799 = !DISubprogram(name: "lroundf", scope: !2571, file: !2571, line: 320, type: !2790, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2800 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2801, file: !2574, line: 1161)
!2801 = !DISubprogram(name: "lroundl", scope: !2571, file: !2571, line: 320, type: !2794, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2802 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2803, file: !2574, line: 1163)
!2803 = !DISubprogram(name: "nan", scope: !2571, file: !2571, line: 201, type: !1663, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2804 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2805, file: !2574, line: 1164)
!2805 = !DISubprogram(name: "nanf", scope: !2571, file: !2571, line: 201, type: !2806, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2806 = !DISubroutineType(types: !2807)
!2807 = !{!1813, !1443}
!2808 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2809, file: !2574, line: 1165)
!2809 = !DISubprogram(name: "nanl", scope: !2571, file: !2571, line: 201, type: !2810, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2810 = !DISubroutineType(types: !2811)
!2811 = !{!1818, !1443}
!2812 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2813, file: !2574, line: 1167)
!2813 = !DISubprogram(name: "nearbyint", scope: !2571, file: !2571, line: 294, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2814 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2815, file: !2574, line: 1168)
!2815 = !DISubprogram(name: "nearbyintf", scope: !2571, file: !2571, line: 294, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2816 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2817, file: !2574, line: 1169)
!2817 = !DISubprogram(name: "nearbyintl", scope: !2571, file: !2571, line: 294, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2818 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2819, file: !2574, line: 1171)
!2819 = !DISubprogram(name: "nextafter", scope: !2571, file: !2571, line: 259, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2820 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2821, file: !2574, line: 1172)
!2821 = !DISubprogram(name: "nextafterf", scope: !2571, file: !2571, line: 259, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2822 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2823, file: !2574, line: 1173)
!2823 = !DISubprogram(name: "nextafterl", scope: !2571, file: !2571, line: 259, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2824 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2825, file: !2574, line: 1175)
!2825 = !DISubprogram(name: "nexttoward", scope: !2571, file: !2571, line: 261, type: !2826, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2826 = !DISubroutineType(types: !2827)
!2827 = !{!1665, !1665, !1818}
!2828 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2829, file: !2574, line: 1176)
!2829 = !DISubprogram(name: "nexttowardf", scope: !2571, file: !2571, line: 261, type: !2830, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2830 = !DISubroutineType(types: !2831)
!2831 = !{!1813, !1813, !1818}
!2832 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2833, file: !2574, line: 1177)
!2833 = !DISubprogram(name: "nexttowardl", scope: !2571, file: !2571, line: 261, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2834 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2835, file: !2574, line: 1179)
!2835 = !DISubprogram(name: "remainder", scope: !2571, file: !2571, line: 272, type: !2581, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2836 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2837, file: !2574, line: 1180)
!2837 = !DISubprogram(name: "remainderf", scope: !2571, file: !2571, line: 272, type: !2664, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2838 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2839, file: !2574, line: 1181)
!2839 = !DISubprogram(name: "remainderl", scope: !2571, file: !2571, line: 272, type: !2668, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2840 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2841, file: !2574, line: 1183)
!2841 = !DISubprogram(name: "remquo", scope: !2571, file: !2571, line: 307, type: !2842, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2842 = !DISubroutineType(types: !2843)
!2843 = !{!1665, !1665, !1665, !2601}
!2844 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2845, file: !2574, line: 1184)
!2845 = !DISubprogram(name: "remquof", scope: !2571, file: !2571, line: 307, type: !2846, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2846 = !DISubroutineType(types: !2847)
!2847 = !{!1813, !1813, !1813, !2601}
!2848 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2849, file: !2574, line: 1185)
!2849 = !DISubprogram(name: "remquol", scope: !2571, file: !2571, line: 307, type: !2850, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2850 = !DISubroutineType(types: !2851)
!2851 = !{!1818, !1818, !1818, !2601}
!2852 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2853, file: !2574, line: 1187)
!2853 = !DISubprogram(name: "rint", scope: !2571, file: !2571, line: 256, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2854 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2855, file: !2574, line: 1188)
!2855 = !DISubprogram(name: "rintf", scope: !2571, file: !2571, line: 256, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2856 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2857, file: !2574, line: 1189)
!2857 = !DISubprogram(name: "rintl", scope: !2571, file: !2571, line: 256, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2858 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2859, file: !2574, line: 1191)
!2859 = !DISubprogram(name: "round", scope: !2571, file: !2571, line: 298, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2860 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2861, file: !2574, line: 1192)
!2861 = !DISubprogram(name: "roundf", scope: !2571, file: !2571, line: 298, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2862 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2863, file: !2574, line: 1193)
!2863 = !DISubprogram(name: "roundl", scope: !2571, file: !2571, line: 298, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2864 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2865, file: !2574, line: 1195)
!2865 = !DISubprogram(name: "scalbln", scope: !2571, file: !2571, line: 290, type: !2866, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2866 = !DISubroutineType(types: !2867)
!2867 = !{!1665, !1665, !1504}
!2868 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2869, file: !2574, line: 1196)
!2869 = !DISubprogram(name: "scalblnf", scope: !2571, file: !2571, line: 290, type: !2870, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2870 = !DISubroutineType(types: !2871)
!2871 = !{!1813, !1813, !1504}
!2872 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2873, file: !2574, line: 1197)
!2873 = !DISubprogram(name: "scalblnl", scope: !2571, file: !2571, line: 290, type: !2874, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2874 = !DISubroutineType(types: !2875)
!2875 = !{!1818, !1818, !1504}
!2876 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2877, file: !2574, line: 1199)
!2877 = !DISubprogram(name: "scalbn", scope: !2571, file: !2571, line: 276, type: !2604, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2878 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2879, file: !2574, line: 1200)
!2879 = !DISubprogram(name: "scalbnf", scope: !2571, file: !2571, line: 276, type: !2880, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2880 = !DISubroutineType(types: !2881)
!2881 = !{!1813, !1813, !93}
!2882 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2883, file: !2574, line: 1201)
!2883 = !DISubprogram(name: "scalbnl", scope: !2571, file: !2571, line: 276, type: !2884, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2884 = !DISubroutineType(types: !2885)
!2885 = !{!1818, !1818, !93}
!2886 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2887, file: !2574, line: 1203)
!2887 = !DISubprogram(name: "tgamma", scope: !2571, file: !2571, line: 235, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2888 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2889, file: !2574, line: 1204)
!2889 = !DISubprogram(name: "tgammaf", scope: !2571, file: !2571, line: 235, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2890 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2891, file: !2574, line: 1205)
!2891 = !DISubprogram(name: "tgammal", scope: !2571, file: !2571, line: 235, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2892 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2893, file: !2574, line: 1207)
!2893 = !DISubprogram(name: "trunc", scope: !2571, file: !2571, line: 302, type: !2572, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2894 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2895, file: !2574, line: 1208)
!2895 = !DISubprogram(name: "truncf", scope: !2571, file: !2571, line: 302, type: !2636, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2896 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !13, entity: !2897, file: !2574, line: 1209)
!2897 = !DISubprogram(name: "truncl", scope: !2571, file: !2571, line: 302, type: !2640, isLocal: false, isDefinition: false, flags: DIFlagPrototyped, isOptimized: true)
!2898 = !DIImportedEntity(tag: DW_TAG_imported_declaration, scope: !2899, entity: !2900, file: !2902, line: 178)
!2899 = !DINamespace(name: "JS", scope: null)
!2900 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "Ok", scope: !20, file: !2901, line: 26, size: 8, flags: DIFlagTypePassByValue, elements: !114, identifier: "_ZTSN7mozilla2OkE")
!2901 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/Result.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2902 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/js/Result.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2903 = !{i32 2, !"Dwarf Version", i32 4}
!2904 = !{i32 2, !"Debug Info Version", i32 3}
!2905 = !{i32 1, !"wchar_size", i32 4}
!2906 = !{i32 7, !"PIC Level", i32 2}
!2907 = !{!"clang version 7.0.0 (tags/RELEASE_700/final)"}
!2908 = distinct !DISubprogram(name: "GetLSBRelease", linkageName: "_ZN7mozilla6widget3lsb13GetLSBReleaseER12nsTSubstringIcES4_S4_S4_", scope: !1570, file: !1, line: 19, type: !2909, isLocal: false, isDefinition: true, scopeLine: 20, flags: DIFlagPrototyped, isOptimized: true, unit: !0, retainedNodes: !2914)
!2909 = !DISubroutineType(types: !2910)
!2910 = !{!117, !2911, !2911, !2911, !2911}
!2911 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !2912, size: 64)
!2912 = !DIDerivedType(tag: DW_TAG_typedef, name: "nsACString", file: !2913, line: 75, baseType: !39)
!2913 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/nsStringFwd.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!2914 = !{!2915, !2916, !2917, !2918, !2919, !2923, !3145, !3499, !3505, !3506, !3572, !3576, !3577, !3578}
!2915 = !DILocalVariable(name: "aDistributor", arg: 1, scope: !2908, file: !1, line: 19, type: !2911)
!2916 = !DILocalVariable(name: "aDescription", arg: 2, scope: !2908, file: !1, line: 19, type: !2911)
!2917 = !DILocalVariable(name: "aRelease", arg: 3, scope: !2908, file: !1, line: 20, type: !2911)
!2918 = !DILocalVariable(name: "aCodename", arg: 4, scope: !2908, file: !1, line: 20, type: !2911)
!2919 = !DILocalVariable(name: "pipefd", scope: !2908, file: !1, line: 23, type: !2920)
!2920 = !DICompositeType(tag: DW_TAG_array_type, baseType: !93, size: 64, elements: !2921)
!2921 = !{!2922}
!2922 = !DISubrange(count: 2)
!2923 = !DILocalVariable(name: "argv", scope: !2908, file: !1, line: 29, type: !2924)
!2924 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "vector<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > >", scope: !13, file: !49, line: 339, size: 192, flags: DIFlagTypePassByReference, elements: !2925, templateParams: !1155, identifier: "_ZTSSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE")
!2925 = !{!2926, !2927, !2931, !2937, !2940, !2946, !2951, !2955, !2958, !2961, !2989, !2990, !2994, !2997, !3000, !3003, !3006, !3011, !3017, !3018, !3019, !3024, !3029, !3030, !3031, !3032, !3033, !3034, !3035, !3038, !3039, !3042, !3043, !3044, !3045, !3048, !3049, !3057, !3064, !3067, !3068, !3069, !3072, !3075, !3076, !3077, !3080, !3083, !3086, !3090, !3091, !3094, !3097, !3100, !3103, !3106, !3109, !3112, !3113, !3114, !3115, !3116, !3119, !3120, !3123, !3124, !3125, !3129, !3133, !3136, !3139, !3142}
!2926 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !2924, baseType: !990, flags: DIFlagProtected, extraData: i32 0)
!2927 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 391, type: !2928, isLocal: false, isDefinition: false, scopeLine: 391, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2928 = !DISubroutineType(types: !2929)
!2929 = !{null, !2930}
!2930 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2924, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!2931 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 402, type: !2932, isLocal: false, isDefinition: false, scopeLine: 402, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!2932 = !DISubroutineType(types: !2933)
!2933 = !{null, !2930, !2934}
!2934 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !2935, size: 64)
!2935 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2936)
!2936 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !2924, file: !49, line: 376, baseType: !1011)
!2937 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 415, type: !2938, isLocal: false, isDefinition: false, scopeLine: 415, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!2938 = !DISubroutineType(types: !2939)
!2939 = !{null, !2930, !1260, !2934}
!2940 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 427, type: !2941, isLocal: false, isDefinition: false, scopeLine: 427, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2941 = !DISubroutineType(types: !2942)
!2942 = !{null, !2930, !1260, !2943, !2934}
!2943 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !2944, size: 64)
!2944 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2945)
!2945 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !2924, file: !49, line: 364, baseType: !31)
!2946 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 458, type: !2947, isLocal: false, isDefinition: false, scopeLine: 458, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2947 = !DISubroutineType(types: !2948)
!2948 = !{null, !2930, !2949}
!2949 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !2950, size: 64)
!2950 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2924)
!2951 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 476, type: !2952, isLocal: false, isDefinition: false, scopeLine: 476, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2952 = !DISubroutineType(types: !2953)
!2953 = !{null, !2930, !2954}
!2954 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !2924, size: 64)
!2955 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 480, type: !2956, isLocal: false, isDefinition: false, scopeLine: 480, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2956 = !DISubroutineType(types: !2957)
!2957 = !{null, !2930, !2949, !2934}
!2958 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 490, type: !2959, isLocal: false, isDefinition: false, scopeLine: 490, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2959 = !DISubroutineType(types: !2960)
!2960 = !{null, !2930, !2954, !2934}
!2961 = !DISubprogram(name: "vector", scope: !2924, file: !49, line: 515, type: !2962, isLocal: false, isDefinition: false, scopeLine: 515, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2962 = !DISubroutineType(types: !2963)
!2963 = !{null, !2930, !2964, !2934}
!2964 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "initializer_list<std::__cxx11::basic_string<char> >", scope: !13, file: !1304, line: 47, size: 128, flags: DIFlagTypePassByValue, elements: !2965, templateParams: !2987, identifier: "_ZTSSt16initializer_listINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE")
!2965 = !{!2966, !2968, !2970, !2975, !2978, !2983, !2986}
!2966 = !DIDerivedType(tag: DW_TAG_member, name: "_M_array", scope: !2964, file: !1304, line: 58, baseType: !2967, size: 64)
!2967 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator", scope: !2964, file: !1304, line: 54, baseType: !1038)
!2968 = !DIDerivedType(tag: DW_TAG_member, name: "_M_len", scope: !2964, file: !1304, line: 59, baseType: !2969, size: 64, offset: 64)
!2969 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", file: !1304, line: 53, baseType: !175)
!2970 = !DISubprogram(name: "initializer_list", scope: !2964, file: !1304, line: 62, type: !2971, isLocal: false, isDefinition: false, scopeLine: 62, flags: DIFlagPrototyped, isOptimized: true)
!2971 = !DISubroutineType(types: !2972)
!2972 = !{null, !2973, !2974, !2969}
!2973 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2964, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!2974 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_iterator", scope: !2964, file: !1304, line: 55, baseType: !1038)
!2975 = !DISubprogram(name: "initializer_list", scope: !2964, file: !1304, line: 66, type: !2976, isLocal: false, isDefinition: false, scopeLine: 66, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2976 = !DISubroutineType(types: !2977)
!2977 = !{null, !2973}
!2978 = !DISubprogram(name: "size", linkageName: "_ZNKSt16initializer_listINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE4sizeEv", scope: !2964, file: !1304, line: 71, type: !2979, isLocal: false, isDefinition: false, scopeLine: 71, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2979 = !DISubroutineType(types: !2980)
!2980 = !{!2969, !2981}
!2981 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2982, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!2982 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !2964)
!2983 = !DISubprogram(name: "begin", linkageName: "_ZNKSt16initializer_listINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE5beginEv", scope: !2964, file: !1304, line: 75, type: !2984, isLocal: false, isDefinition: false, scopeLine: 75, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2984 = !DISubroutineType(types: !2985)
!2985 = !{!2974, !2981}
!2986 = !DISubprogram(name: "end", linkageName: "_ZNKSt16initializer_listINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE3endEv", scope: !2964, file: !1304, line: 79, type: !2984, isLocal: false, isDefinition: false, scopeLine: 79, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2987 = !{!2988}
!2988 = !DITemplateTypeParameter(name: "_E", type: !31)
!2989 = !DISubprogram(name: "~vector", scope: !2924, file: !49, line: 565, type: !2928, isLocal: false, isDefinition: false, scopeLine: 565, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2990 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEaSERKS7_", scope: !2924, file: !49, line: 582, type: !2991, isLocal: false, isDefinition: false, scopeLine: 582, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2991 = !DISubroutineType(types: !2992)
!2992 = !{!2993, !2930, !2949}
!2993 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !2924, size: 64)
!2994 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEaSEOS7_", scope: !2924, file: !49, line: 596, type: !2995, isLocal: false, isDefinition: false, scopeLine: 596, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2995 = !DISubroutineType(types: !2996)
!2996 = !{!2993, !2930, !2954}
!2997 = !DISubprogram(name: "operator=", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEaSESt16initializer_listIS5_E", scope: !2924, file: !49, line: 617, type: !2998, isLocal: false, isDefinition: false, scopeLine: 617, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!2998 = !DISubroutineType(types: !2999)
!2999 = !{!2993, !2930, !2964}
!3000 = !DISubprogram(name: "assign", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6assignEmRKS5_", scope: !2924, file: !49, line: 636, type: !3001, isLocal: false, isDefinition: false, scopeLine: 636, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3001 = !DISubroutineType(types: !3002)
!3002 = !{null, !2930, !1260, !2943}
!3003 = !DISubprogram(name: "assign", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6assignESt16initializer_listIS5_E", scope: !2924, file: !49, line: 681, type: !3004, isLocal: false, isDefinition: false, scopeLine: 681, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3004 = !DISubroutineType(types: !3005)
!3005 = !{null, !2930, !2964}
!3006 = !DISubprogram(name: "begin", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5beginEv", scope: !2924, file: !49, line: 698, type: !3007, isLocal: false, isDefinition: false, scopeLine: 698, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3007 = !DISubroutineType(types: !3008)
!3008 = !{!3009, !2930}
!3009 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator", scope: !2924, file: !49, line: 369, baseType: !3010)
!3010 = !DICompositeType(tag: DW_TAG_class_type, name: "__normal_iterator<std::__cxx11::basic_string<char> *, std::vector<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > > >", scope: !5, file: !879, line: 764, flags: DIFlagFwdDecl, identifier: "_ZTSN9__gnu_cxx17__normal_iteratorIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt6vectorIS6_SaIS6_EEEE")
!3011 = !DISubprogram(name: "begin", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5beginEv", scope: !2924, file: !49, line: 707, type: !3012, isLocal: false, isDefinition: false, scopeLine: 707, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3012 = !DISubroutineType(types: !3013)
!3013 = !{!3014, !3016}
!3014 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_iterator", scope: !2924, file: !49, line: 371, baseType: !3015)
!3015 = !DICompositeType(tag: DW_TAG_class_type, name: "__normal_iterator<const std::__cxx11::basic_string<char> *, std::vector<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > > >", scope: !5, file: !879, line: 764, flags: DIFlagFwdDecl, identifier: "_ZTSN9__gnu_cxx17__normal_iteratorIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt6vectorIS6_SaIS6_EEEE")
!3016 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2950, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3017 = !DISubprogram(name: "end", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE3endEv", scope: !2924, file: !49, line: 716, type: !3007, isLocal: false, isDefinition: false, scopeLine: 716, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3018 = !DISubprogram(name: "end", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE3endEv", scope: !2924, file: !49, line: 725, type: !3012, isLocal: false, isDefinition: false, scopeLine: 725, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3019 = !DISubprogram(name: "rbegin", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6rbeginEv", scope: !2924, file: !49, line: 734, type: !3020, isLocal: false, isDefinition: false, scopeLine: 734, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3020 = !DISubroutineType(types: !3021)
!3021 = !{!3022, !2930}
!3022 = !DIDerivedType(tag: DW_TAG_typedef, name: "reverse_iterator", scope: !2924, file: !49, line: 373, baseType: !3023)
!3023 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<__gnu_cxx::__normal_iterator<std::__cxx11::basic_string<char> *, std::vector<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > > > >", scope: !13, file: !879, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorIN9__gnu_cxx17__normal_iteratorIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt6vectorIS7_SaIS7_EEEEE")
!3024 = !DISubprogram(name: "rbegin", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6rbeginEv", scope: !2924, file: !49, line: 743, type: !3025, isLocal: false, isDefinition: false, scopeLine: 743, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3025 = !DISubroutineType(types: !3026)
!3026 = !{!3027, !3016}
!3027 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reverse_iterator", scope: !2924, file: !49, line: 372, baseType: !3028)
!3028 = !DICompositeType(tag: DW_TAG_class_type, name: "reverse_iterator<__gnu_cxx::__normal_iterator<const std::__cxx11::basic_string<char> *, std::vector<std::__cxx11::basic_string<char>, std::allocator<std::__cxx11::basic_string<char> > > > >", scope: !13, file: !879, line: 101, flags: DIFlagFwdDecl, identifier: "_ZTSSt16reverse_iteratorIN9__gnu_cxx17__normal_iteratorIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt6vectorIS7_SaIS7_EEEEE")
!3029 = !DISubprogram(name: "rend", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4rendEv", scope: !2924, file: !49, line: 752, type: !3020, isLocal: false, isDefinition: false, scopeLine: 752, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3030 = !DISubprogram(name: "rend", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4rendEv", scope: !2924, file: !49, line: 761, type: !3025, isLocal: false, isDefinition: false, scopeLine: 761, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3031 = !DISubprogram(name: "cbegin", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6cbeginEv", scope: !2924, file: !49, line: 771, type: !3012, isLocal: false, isDefinition: false, scopeLine: 771, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3032 = !DISubprogram(name: "cend", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4cendEv", scope: !2924, file: !49, line: 780, type: !3012, isLocal: false, isDefinition: false, scopeLine: 780, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3033 = !DISubprogram(name: "crbegin", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE7crbeginEv", scope: !2924, file: !49, line: 789, type: !3025, isLocal: false, isDefinition: false, scopeLine: 789, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3034 = !DISubprogram(name: "crend", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5crendEv", scope: !2924, file: !49, line: 798, type: !3025, isLocal: false, isDefinition: false, scopeLine: 798, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3035 = !DISubprogram(name: "size", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4sizeEv", scope: !2924, file: !49, line: 805, type: !3036, isLocal: false, isDefinition: false, scopeLine: 805, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3036 = !DISubroutineType(types: !3037)
!3037 = !{!1260, !3016}
!3038 = !DISubprogram(name: "max_size", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE8max_sizeEv", scope: !2924, file: !49, line: 810, type: !3036, isLocal: false, isDefinition: false, scopeLine: 810, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3039 = !DISubprogram(name: "resize", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6resizeEm", scope: !2924, file: !49, line: 824, type: !3040, isLocal: false, isDefinition: false, scopeLine: 824, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3040 = !DISubroutineType(types: !3041)
!3041 = !{null, !2930, !1260}
!3042 = !DISubprogram(name: "resize", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6resizeEmRKS5_", scope: !2924, file: !49, line: 844, type: !3001, isLocal: false, isDefinition: false, scopeLine: 844, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3043 = !DISubprogram(name: "shrink_to_fit", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE13shrink_to_fitEv", scope: !2924, file: !49, line: 876, type: !2928, isLocal: false, isDefinition: false, scopeLine: 876, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3044 = !DISubprogram(name: "capacity", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE8capacityEv", scope: !2924, file: !49, line: 885, type: !3036, isLocal: false, isDefinition: false, scopeLine: 885, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3045 = !DISubprogram(name: "empty", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5emptyEv", scope: !2924, file: !49, line: 894, type: !3046, isLocal: false, isDefinition: false, scopeLine: 894, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3046 = !DISubroutineType(types: !3047)
!3047 = !{!117, !3016}
!3048 = !DISubprogram(name: "reserve", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE7reserveEm", scope: !2924, file: !49, line: 915, type: !3040, isLocal: false, isDefinition: false, scopeLine: 915, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3049 = !DISubprogram(name: "operator[]", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEixEm", scope: !2924, file: !49, line: 930, type: !3050, isLocal: false, isDefinition: false, scopeLine: 930, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3050 = !DISubroutineType(types: !3051)
!3051 = !{!3052, !2930, !1260}
!3052 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !2924, file: !49, line: 367, baseType: !3053)
!3053 = !DIDerivedType(tag: DW_TAG_typedef, name: "reference", scope: !999, file: !59, line: 64, baseType: !3054)
!3054 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3055, size: 64)
!3055 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !999, file: !59, line: 58, baseType: !3056)
!3056 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !1002, file: !64, line: 389, baseType: !31)
!3057 = !DISubprogram(name: "operator[]", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEixEm", scope: !2924, file: !49, line: 948, type: !3058, isLocal: false, isDefinition: false, scopeLine: 948, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3058 = !DISubroutineType(types: !3059)
!3059 = !{!3060, !3016, !1260}
!3060 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !2924, file: !49, line: 368, baseType: !3061)
!3061 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reference", scope: !999, file: !59, line: 65, baseType: !3062)
!3062 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3063, size: 64)
!3063 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3055)
!3064 = !DISubprogram(name: "_M_range_check", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_range_checkEm", scope: !2924, file: !49, line: 957, type: !3065, isLocal: false, isDefinition: false, scopeLine: 957, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3065 = !DISubroutineType(types: !3066)
!3066 = !{null, !3016, !1260}
!3067 = !DISubprogram(name: "at", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE2atEm", scope: !2924, file: !49, line: 979, type: !3050, isLocal: false, isDefinition: false, scopeLine: 979, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3068 = !DISubprogram(name: "at", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE2atEm", scope: !2924, file: !49, line: 997, type: !3058, isLocal: false, isDefinition: false, scopeLine: 997, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3069 = !DISubprogram(name: "front", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5frontEv", scope: !2924, file: !49, line: 1008, type: !3070, isLocal: false, isDefinition: false, scopeLine: 1008, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3070 = !DISubroutineType(types: !3071)
!3071 = !{!3052, !2930}
!3072 = !DISubprogram(name: "front", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5frontEv", scope: !2924, file: !49, line: 1019, type: !3073, isLocal: false, isDefinition: false, scopeLine: 1019, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3073 = !DISubroutineType(types: !3074)
!3074 = !{!3060, !3016}
!3075 = !DISubprogram(name: "back", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4backEv", scope: !2924, file: !49, line: 1030, type: !3070, isLocal: false, isDefinition: false, scopeLine: 1030, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3076 = !DISubprogram(name: "back", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4backEv", scope: !2924, file: !49, line: 1041, type: !3073, isLocal: false, isDefinition: false, scopeLine: 1041, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3077 = !DISubprogram(name: "data", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4dataEv", scope: !2924, file: !49, line: 1055, type: !3078, isLocal: false, isDefinition: false, scopeLine: 1055, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3078 = !DISubroutineType(types: !3079)
!3079 = !{!1008, !2930}
!3080 = !DISubprogram(name: "data", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4dataEv", scope: !2924, file: !49, line: 1059, type: !3081, isLocal: false, isDefinition: false, scopeLine: 1059, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3081 = !DISubroutineType(types: !3082)
!3082 = !{!1038, !3016}
!3083 = !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE9push_backERKS5_", scope: !2924, file: !49, line: 1074, type: !3084, isLocal: false, isDefinition: false, scopeLine: 1074, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3084 = !DISubroutineType(types: !3085)
!3085 = !{null, !2930, !2943}
!3086 = !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE9push_backEOS5_", scope: !2924, file: !49, line: 1090, type: !3087, isLocal: false, isDefinition: false, scopeLine: 1090, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3087 = !DISubroutineType(types: !3088)
!3088 = !{null, !2930, !3089}
!3089 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !2945, size: 64)
!3090 = !DISubprogram(name: "pop_back", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE8pop_backEv", scope: !2924, file: !49, line: 1112, type: !2928, isLocal: false, isDefinition: false, scopeLine: 1112, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3091 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EERSA_", scope: !2924, file: !49, line: 1150, type: !3092, isLocal: false, isDefinition: false, scopeLine: 1150, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3092 = !DISubroutineType(types: !3093)
!3093 = !{!3009, !2930, !3014, !2943}
!3094 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EEOS5_", scope: !2924, file: !49, line: 1180, type: !3095, isLocal: false, isDefinition: false, scopeLine: 1180, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3095 = !DISubroutineType(types: !3096)
!3096 = !{!3009, !2930, !3014, !3089}
!3097 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EESt16initializer_listIS5_E", scope: !2924, file: !49, line: 1197, type: !3098, isLocal: false, isDefinition: false, scopeLine: 1197, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3098 = !DISubroutineType(types: !3099)
!3099 = !{!3009, !2930, !3014, !2964}
!3100 = !DISubprogram(name: "insert", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE6insertEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EEmRSA_", scope: !2924, file: !49, line: 1222, type: !3101, isLocal: false, isDefinition: false, scopeLine: 1222, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3101 = !DISubroutineType(types: !3102)
!3102 = !{!3009, !2930, !3014, !1260, !2943}
!3103 = !DISubprogram(name: "erase", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5eraseEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EE", scope: !2924, file: !49, line: 1317, type: !3104, isLocal: false, isDefinition: false, scopeLine: 1317, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3104 = !DISubroutineType(types: !3105)
!3105 = !{!3009, !2930, !3014}
!3106 = !DISubprogram(name: "erase", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5eraseEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EESC_", scope: !2924, file: !49, line: 1344, type: !3107, isLocal: false, isDefinition: false, scopeLine: 1344, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3107 = !DISubroutineType(types: !3108)
!3108 = !{!3009, !2930, !3014, !3014}
!3109 = !DISubprogram(name: "swap", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE4swapERS7_", scope: !2924, file: !49, line: 1367, type: !3110, isLocal: false, isDefinition: false, scopeLine: 1367, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3110 = !DISubroutineType(types: !3111)
!3111 = !{null, !2930, !2993}
!3112 = !DISubprogram(name: "clear", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE5clearEv", scope: !2924, file: !49, line: 1385, type: !2928, isLocal: false, isDefinition: false, scopeLine: 1385, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3113 = !DISubprogram(name: "_M_fill_initialize", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE18_M_fill_initializeEmRKS5_", scope: !2924, file: !49, line: 1477, type: !3001, isLocal: false, isDefinition: false, scopeLine: 1477, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3114 = !DISubprogram(name: "_M_default_initialize", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE21_M_default_initializeEm", scope: !2924, file: !49, line: 1487, type: !3040, isLocal: false, isDefinition: false, scopeLine: 1487, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3115 = !DISubprogram(name: "_M_fill_assign", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_fill_assignEmRKS5_", scope: !2924, file: !49, line: 1529, type: !3001, isLocal: false, isDefinition: false, scopeLine: 1529, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3116 = !DISubprogram(name: "_M_fill_insert", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_fill_insertEN9__gnu_cxx17__normal_iteratorIPS5_S7_EEmRKS5_", scope: !2924, file: !49, line: 1568, type: !3117, isLocal: false, isDefinition: false, scopeLine: 1568, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3117 = !DISubroutineType(types: !3118)
!3118 = !{null, !2930, !3009, !1260, !2943}
!3119 = !DISubprogram(name: "_M_default_append", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE17_M_default_appendEm", scope: !2924, file: !49, line: 1573, type: !3040, isLocal: false, isDefinition: false, scopeLine: 1573, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3120 = !DISubprogram(name: "_M_shrink_to_fit", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE16_M_shrink_to_fitEv", scope: !2924, file: !49, line: 1576, type: !3121, isLocal: false, isDefinition: false, scopeLine: 1576, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3121 = !DISubroutineType(types: !3122)
!3122 = !{!117, !2930}
!3123 = !DISubprogram(name: "_M_insert_rval", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_insert_rvalEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EEOS5_", scope: !2924, file: !49, line: 1625, type: !3095, isLocal: false, isDefinition: false, scopeLine: 1625, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3124 = !DISubprogram(name: "_M_emplace_aux", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_emplace_auxEN9__gnu_cxx17__normal_iteratorIPKS5_S7_EEOS5_", scope: !2924, file: !49, line: 1634, type: !3095, isLocal: false, isDefinition: false, scopeLine: 1634, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3125 = !DISubprogram(name: "_M_check_len", linkageName: "_ZNKSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE12_M_check_lenEmPKc", scope: !2924, file: !49, line: 1640, type: !3126, isLocal: false, isDefinition: false, scopeLine: 1640, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3126 = !DISubroutineType(types: !3127)
!3127 = !{!3128, !3016, !1260, !1443}
!3128 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !2924, file: !49, line: 374, baseType: !175)
!3129 = !DISubprogram(name: "_M_erase_at_end", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE15_M_erase_at_endEPS5_", scope: !2924, file: !49, line: 1654, type: !3130, isLocal: false, isDefinition: false, scopeLine: 1654, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3130 = !DISubroutineType(types: !3131)
!3131 = !{null, !2930, !3132}
!3132 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !2924, file: !49, line: 365, baseType: !997)
!3133 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE8_M_eraseEN9__gnu_cxx17__normal_iteratorIPS5_S7_EE", scope: !2924, file: !49, line: 1666, type: !3134, isLocal: false, isDefinition: false, scopeLine: 1666, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3134 = !DISubroutineType(types: !3135)
!3135 = !{!3009, !2930, !3009}
!3136 = !DISubprogram(name: "_M_erase", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE8_M_eraseEN9__gnu_cxx17__normal_iteratorIPS5_S7_EESB_", scope: !2924, file: !49, line: 1669, type: !3137, isLocal: false, isDefinition: false, scopeLine: 1669, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3137 = !DISubroutineType(types: !3138)
!3138 = !{!3009, !2930, !3009, !3009}
!3139 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_move_assignEOS7_St17integral_constantIbLb1EE", scope: !2924, file: !49, line: 1677, type: !3140, isLocal: false, isDefinition: false, scopeLine: 1677, flags: DIFlagPrototyped, isOptimized: true)
!3140 = !DISubroutineType(types: !3141)
!3141 = !{null, !2930, !2954, !950}
!3142 = !DISubprogram(name: "_M_move_assign", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE14_M_move_assignEOS7_St17integral_constantIbLb0EE", scope: !2924, file: !49, line: 1688, type: !3143, isLocal: false, isDefinition: false, scopeLine: 1688, flags: DIFlagPrototyped, isOptimized: true)
!3143 = !DISubroutineType(types: !3144)
!3144 = !{null, !2930, !2954, !968}
!3145 = !DILocalVariable(name: "options", scope: !2908, file: !1, line: 31, type: !3146)
!3146 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "LaunchOptions", scope: !3148, file: !3147, line: 96, size: 704, flags: DIFlagTypePassByReference, elements: !3149, identifier: "_ZTSN4base13LaunchOptionsE")
!3147 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/ipc/chromium/src/base/process_util.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3148 = !DINamespace(name: "base", scope: null)
!3149 = !{!3150, !3151, !3322, !3324}
!3150 = !DIDerivedType(tag: DW_TAG_member, name: "wait", scope: !3146, file: !3147, line: 99, baseType: !117, size: 8)
!3151 = !DIDerivedType(tag: DW_TAG_member, name: "env_map", scope: !3146, file: !3147, line: 111, baseType: !3152, size: 384, offset: 64)
!3152 = !DIDerivedType(tag: DW_TAG_typedef, name: "environment_map", scope: !3148, file: !3147, line: 93, baseType: !3153)
!3153 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "map<std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char>, std::less<std::__cxx11::basic_string<char> >, std::allocator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > >", scope: !13, file: !3154, line: 100, size: 384, flags: DIFlagTypePassByReference, elements: !3155, templateParams: !3321, identifier: "_ZTSSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE")
!3154 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_map.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3155 = !{!3156, !3158, !3162, !3168, !3173, !3177, !3181, !3184, !3187, !3190, !3193, !3194, !3198, !3201, !3204, !3208, !3212, !3216, !3217, !3218, !3222, !3226, !3227, !3228, !3229, !3230, !3231, !3232, !3235, !3239, !3240, !3248, !3252, !3253, !3258, !3265, !3269, !3272, !3275, !3278, !3281, !3284, !3287, !3290, !3293, !3294, !3298, !3302, !3305, !3308, !3311, !3312, !3313, !3314, !3315, !3318}
!3156 = !DIDerivedType(tag: DW_TAG_member, name: "_M_t", scope: !3153, file: !3154, line: 151, baseType: !3157, size: 384)
!3157 = !DIDerivedType(tag: DW_TAG_typedef, name: "_Rep_type", scope: !3153, file: !3154, line: 148, baseType: !303)
!3158 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 183, type: !3159, isLocal: false, isDefinition: false, scopeLine: 183, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3159 = !DISubroutineType(types: !3160)
!3160 = !{null, !3161}
!3161 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3153, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3162 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 192, type: !3163, isLocal: false, isDefinition: false, scopeLine: 192, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3163 = !DISubroutineType(types: !3164)
!3164 = !{null, !3161, !610, !3165}
!3165 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3166, size: 64)
!3166 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3167)
!3167 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !3153, file: !3154, line: 107, baseType: !381)
!3168 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 205, type: !3169, isLocal: false, isDefinition: false, scopeLine: 205, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3169 = !DISubroutineType(types: !3170)
!3170 = !{null, !3161, !3171}
!3171 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3172, size: 64)
!3172 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3153)
!3173 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 213, type: !3174, isLocal: false, isDefinition: false, scopeLine: 213, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3174 = !DISubroutineType(types: !3175)
!3175 = !{null, !3161, !3176}
!3176 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3153, size: 64)
!3177 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 226, type: !3178, isLocal: false, isDefinition: false, scopeLine: 226, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3178 = !DISubroutineType(types: !3179)
!3179 = !{null, !3161, !3180, !610, !3165}
!3180 = !DICompositeType(tag: DW_TAG_class_type, name: "initializer_list<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", scope: !13, file: !1304, line: 47, flags: DIFlagFwdDecl, identifier: "_ZTSSt16initializer_listISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE")
!3181 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 234, type: !3182, isLocal: false, isDefinition: false, scopeLine: 234, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3182 = !DISubroutineType(types: !3183)
!3183 = !{null, !3161, !3165}
!3184 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 238, type: !3185, isLocal: false, isDefinition: false, scopeLine: 238, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3185 = !DISubroutineType(types: !3186)
!3186 = !{null, !3161, !3171, !3165}
!3187 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 242, type: !3188, isLocal: false, isDefinition: false, scopeLine: 242, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3188 = !DISubroutineType(types: !3189)
!3189 = !{null, !3161, !3176, !3165}
!3190 = !DISubprogram(name: "map", scope: !3153, file: !3154, line: 248, type: !3191, isLocal: false, isDefinition: false, scopeLine: 248, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3191 = !DISubroutineType(types: !3192)
!3192 = !{null, !3161, !3180, !3165}
!3193 = !DISubprogram(name: "~map", scope: !3153, file: !3154, line: 300, type: !3159, isLocal: false, isDefinition: false, scopeLine: 300, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3194 = !DISubprogram(name: "operator=", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEEaSERKSC_", scope: !3153, file: !3154, line: 317, type: !3195, isLocal: false, isDefinition: false, scopeLine: 317, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3195 = !DISubroutineType(types: !3196)
!3196 = !{!3197, !3161, !3171}
!3197 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3153, size: 64)
!3198 = !DISubprogram(name: "operator=", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEEaSEOSC_", scope: !3153, file: !3154, line: 321, type: !3199, isLocal: false, isDefinition: false, scopeLine: 321, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3199 = !DISubroutineType(types: !3200)
!3200 = !{!3197, !3161, !3176}
!3201 = !DISubprogram(name: "operator=", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEEaSESt16initializer_listISA_E", scope: !3153, file: !3154, line: 335, type: !3202, isLocal: false, isDefinition: false, scopeLine: 335, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3202 = !DISubroutineType(types: !3203)
!3203 = !{!3197, !3161, !3180}
!3204 = !DISubprogram(name: "get_allocator", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE13get_allocatorEv", scope: !3153, file: !3154, line: 344, type: !3205, isLocal: false, isDefinition: false, scopeLine: 344, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3205 = !DISubroutineType(types: !3206)
!3206 = !{!3167, !3207}
!3207 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3172, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3208 = !DISubprogram(name: "begin", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5beginEv", scope: !3153, file: !3154, line: 354, type: !3209, isLocal: false, isDefinition: false, scopeLine: 354, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3209 = !DISubroutineType(types: !3210)
!3210 = !{!3211, !3161}
!3211 = !DIDerivedType(tag: DW_TAG_typedef, name: "iterator", scope: !3153, file: !3154, line: 162, baseType: !810)
!3212 = !DISubprogram(name: "begin", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5beginEv", scope: !3153, file: !3154, line: 363, type: !3213, isLocal: false, isDefinition: false, scopeLine: 363, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3213 = !DISubroutineType(types: !3214)
!3214 = !{!3215, !3207}
!3215 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_iterator", scope: !3153, file: !3154, line: 163, baseType: !804)
!3216 = !DISubprogram(name: "end", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE3endEv", scope: !3153, file: !3154, line: 372, type: !3209, isLocal: false, isDefinition: false, scopeLine: 372, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3217 = !DISubprogram(name: "end", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE3endEv", scope: !3153, file: !3154, line: 381, type: !3213, isLocal: false, isDefinition: false, scopeLine: 381, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3218 = !DISubprogram(name: "rbegin", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6rbeginEv", scope: !3153, file: !3154, line: 390, type: !3219, isLocal: false, isDefinition: false, scopeLine: 390, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3219 = !DISubroutineType(types: !3220)
!3220 = !{!3221, !3161}
!3221 = !DIDerivedType(tag: DW_TAG_typedef, name: "reverse_iterator", scope: !3153, file: !3154, line: 166, baseType: !877)
!3222 = !DISubprogram(name: "rbegin", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6rbeginEv", scope: !3153, file: !3154, line: 399, type: !3223, isLocal: false, isDefinition: false, scopeLine: 399, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3223 = !DISubroutineType(types: !3224)
!3224 = !{!3225, !3207}
!3225 = !DIDerivedType(tag: DW_TAG_typedef, name: "const_reverse_iterator", scope: !3153, file: !3154, line: 167, baseType: !883)
!3226 = !DISubprogram(name: "rend", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4rendEv", scope: !3153, file: !3154, line: 408, type: !3219, isLocal: false, isDefinition: false, scopeLine: 408, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3227 = !DISubprogram(name: "rend", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4rendEv", scope: !3153, file: !3154, line: 417, type: !3223, isLocal: false, isDefinition: false, scopeLine: 417, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3228 = !DISubprogram(name: "cbegin", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6cbeginEv", scope: !3153, file: !3154, line: 427, type: !3213, isLocal: false, isDefinition: false, scopeLine: 427, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3229 = !DISubprogram(name: "cend", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4cendEv", scope: !3153, file: !3154, line: 436, type: !3213, isLocal: false, isDefinition: false, scopeLine: 436, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3230 = !DISubprogram(name: "crbegin", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE7crbeginEv", scope: !3153, file: !3154, line: 445, type: !3223, isLocal: false, isDefinition: false, scopeLine: 445, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3231 = !DISubprogram(name: "crend", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5crendEv", scope: !3153, file: !3154, line: 454, type: !3223, isLocal: false, isDefinition: false, scopeLine: 454, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3232 = !DISubprogram(name: "empty", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5emptyEv", scope: !3153, file: !3154, line: 463, type: !3233, isLocal: false, isDefinition: false, scopeLine: 463, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3233 = !DISubroutineType(types: !3234)
!3234 = !{!117, !3207}
!3235 = !DISubprogram(name: "size", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4sizeEv", scope: !3153, file: !3154, line: 468, type: !3236, isLocal: false, isDefinition: false, scopeLine: 468, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3236 = !DISubroutineType(types: !3237)
!3237 = !{!3238, !3207}
!3238 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !3153, file: !3154, line: 164, baseType: !913)
!3239 = !DISubprogram(name: "max_size", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE8max_sizeEv", scope: !3153, file: !3154, line: 473, type: !3236, isLocal: false, isDefinition: false, scopeLine: 473, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3240 = !DISubprogram(name: "operator[]", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEEixERS9_", scope: !3153, file: !3154, line: 490, type: !3241, isLocal: false, isDefinition: false, scopeLine: 490, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3241 = !DISubroutineType(types: !3242)
!3242 = !{!3243, !3161, !3245}
!3243 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3244, size: 64)
!3244 = !DIDerivedType(tag: DW_TAG_typedef, name: "mapped_type", scope: !3153, file: !3154, line: 104, baseType: !31)
!3245 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3246, size: 64)
!3246 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3247)
!3247 = !DIDerivedType(tag: DW_TAG_typedef, name: "key_type", scope: !3153, file: !3154, line: 103, baseType: !31)
!3248 = !DISubprogram(name: "operator[]", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEEixEOS5_", scope: !3153, file: !3154, line: 510, type: !3249, isLocal: false, isDefinition: false, scopeLine: 510, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3249 = !DISubroutineType(types: !3250)
!3250 = !{!3243, !3161, !3251}
!3251 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3247, size: 64)
!3252 = !DISubprogram(name: "at", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE2atERS9_", scope: !3153, file: !3154, line: 535, type: !3241, isLocal: false, isDefinition: false, scopeLine: 535, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3253 = !DISubprogram(name: "at", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE2atERS9_", scope: !3153, file: !3154, line: 544, type: !3254, isLocal: false, isDefinition: false, scopeLine: 544, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3254 = !DISubroutineType(types: !3255)
!3255 = !{!3256, !3207, !3245}
!3256 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3257, size: 64)
!3257 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3244)
!3258 = !DISubprogram(name: "insert", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6insertERKSA_", scope: !3153, file: !3154, line: 801, type: !3259, isLocal: false, isDefinition: false, scopeLine: 801, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3259 = !DISubroutineType(types: !3260)
!3260 = !{!3261, !3161, !3262}
!3261 = !DICompositeType(tag: DW_TAG_structure_type, name: "pair<std::_Rb_tree_iterator<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >, bool>", scope: !13, file: !72, line: 208, flags: DIFlagFwdDecl, identifier: "_ZTSSt4pairISt17_Rb_tree_iteratorIS_IKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EEbE")
!3262 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3263, size: 64)
!3263 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3264)
!3264 = !DIDerivedType(tag: DW_TAG_typedef, name: "value_type", scope: !3153, file: !3154, line: 105, baseType: !322)
!3265 = !DISubprogram(name: "insert", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6insertEOSA_", scope: !3153, file: !3154, line: 808, type: !3266, isLocal: false, isDefinition: false, scopeLine: 808, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3266 = !DISubroutineType(types: !3267)
!3267 = !{!3261, !3161, !3268}
!3268 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3264, size: 64)
!3269 = !DISubprogram(name: "insert", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6insertESt16initializer_listISA_E", scope: !3153, file: !3154, line: 828, type: !3270, isLocal: false, isDefinition: false, scopeLine: 828, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3270 = !DISubroutineType(types: !3271)
!3271 = !{null, !3161, !3180}
!3272 = !DISubprogram(name: "insert", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6insertESt23_Rb_tree_const_iteratorISA_ERKSA_", scope: !3153, file: !3154, line: 858, type: !3273, isLocal: false, isDefinition: false, scopeLine: 858, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3273 = !DISubroutineType(types: !3274)
!3274 = !{!3211, !3161, !3215, !3262}
!3275 = !DISubprogram(name: "insert", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE6insertESt23_Rb_tree_const_iteratorISA_EOSA_", scope: !3153, file: !3154, line: 868, type: !3276, isLocal: false, isDefinition: false, scopeLine: 868, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3276 = !DISubroutineType(types: !3277)
!3277 = !{!3211, !3161, !3215, !3268}
!3278 = !DISubprogram(name: "erase", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5eraseESt23_Rb_tree_const_iteratorISA_E", scope: !3153, file: !3154, line: 1030, type: !3279, isLocal: false, isDefinition: false, scopeLine: 1030, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3279 = !DISubroutineType(types: !3280)
!3280 = !{!3211, !3161, !3215}
!3281 = !DISubprogram(name: "erase", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5eraseB5cxx11ESt17_Rb_tree_iteratorISA_E", scope: !3153, file: !3154, line: 1036, type: !3282, isLocal: false, isDefinition: false, scopeLine: 1036, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3282 = !DISubroutineType(types: !3283)
!3283 = !{!3211, !3161, !3211}
!3284 = !DISubprogram(name: "erase", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5eraseERS9_", scope: !3153, file: !3154, line: 1067, type: !3285, isLocal: false, isDefinition: false, scopeLine: 1067, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3285 = !DISubroutineType(types: !3286)
!3286 = !{!3238, !3161, !3245}
!3287 = !DISubprogram(name: "erase", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5eraseESt23_Rb_tree_const_iteratorISA_ESE_", scope: !3153, file: !3154, line: 1087, type: !3288, isLocal: false, isDefinition: false, scopeLine: 1087, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3288 = !DISubroutineType(types: !3289)
!3289 = !{!3211, !3161, !3215, !3215}
!3290 = !DISubprogram(name: "swap", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4swapERSC_", scope: !3153, file: !3154, line: 1121, type: !3291, isLocal: false, isDefinition: false, scopeLine: 1121, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3291 = !DISubroutineType(types: !3292)
!3292 = !{null, !3161, !3197}
!3293 = !DISubprogram(name: "clear", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5clearEv", scope: !3153, file: !3154, line: 1132, type: !3159, isLocal: false, isDefinition: false, scopeLine: 1132, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3294 = !DISubprogram(name: "key_comp", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE8key_compEv", scope: !3153, file: !3154, line: 1141, type: !3295, isLocal: false, isDefinition: false, scopeLine: 1141, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3295 = !DISubroutineType(types: !3296)
!3296 = !{!3297, !3207}
!3297 = !DIDerivedType(tag: DW_TAG_typedef, name: "key_compare", scope: !3153, file: !3154, line: 106, baseType: !586)
!3298 = !DISubprogram(name: "value_comp", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE10value_compEv", scope: !3153, file: !3154, line: 1149, type: !3299, isLocal: false, isDefinition: false, scopeLine: 1149, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3299 = !DISubroutineType(types: !3300)
!3300 = !{!3301, !3207}
!3301 = !DICompositeType(tag: DW_TAG_class_type, name: "value_compare", scope: !3153, file: !3154, line: 127, flags: DIFlagFwdDecl, identifier: "_ZTSNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE13value_compareE")
!3302 = !DISubprogram(name: "find", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4findERS9_", scope: !3153, file: !3154, line: 1168, type: !3303, isLocal: false, isDefinition: false, scopeLine: 1168, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3303 = !DISubroutineType(types: !3304)
!3304 = !{!3211, !3161, !3245}
!3305 = !DISubprogram(name: "find", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE4findERS9_", scope: !3153, file: !3154, line: 1193, type: !3306, isLocal: false, isDefinition: false, scopeLine: 1193, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3306 = !DISubroutineType(types: !3307)
!3307 = !{!3215, !3207, !3245}
!3308 = !DISubprogram(name: "count", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE5countERS9_", scope: !3153, file: !3154, line: 1214, type: !3309, isLocal: false, isDefinition: false, scopeLine: 1214, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3309 = !DISubroutineType(types: !3310)
!3310 = !{!3238, !3207, !3245}
!3311 = !DISubprogram(name: "lower_bound", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE11lower_boundERS9_", scope: !3153, file: !3154, line: 1238, type: !3303, isLocal: false, isDefinition: false, scopeLine: 1238, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3312 = !DISubprogram(name: "lower_bound", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE11lower_boundERS9_", scope: !3153, file: !3154, line: 1263, type: !3306, isLocal: false, isDefinition: false, scopeLine: 1263, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3313 = !DISubprogram(name: "upper_bound", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE11upper_boundERS9_", scope: !3153, file: !3154, line: 1283, type: !3303, isLocal: false, isDefinition: false, scopeLine: 1283, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3314 = !DISubprogram(name: "upper_bound", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE11upper_boundERS9_", scope: !3153, file: !3154, line: 1303, type: !3306, isLocal: false, isDefinition: false, scopeLine: 1303, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3315 = !DISubprogram(name: "equal_range", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE11equal_rangeERS9_", scope: !3153, file: !3154, line: 1332, type: !3316, isLocal: false, isDefinition: false, scopeLine: 1332, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3316 = !DISubroutineType(types: !3317)
!3317 = !{!938, !3161, !3245}
!3318 = !DISubprogram(name: "equal_range", linkageName: "_ZNKSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEE11equal_rangeERS9_", scope: !3153, file: !3154, line: 1361, type: !3319, isLocal: false, isDefinition: false, scopeLine: 1361, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3319 = !DISubroutineType(types: !3320)
!3320 = !{!942, !3207, !3245}
!3321 = !{!984, !602, !987, !446}
!3322 = !DIDerivedType(tag: DW_TAG_member, name: "fds_to_remap", scope: !3146, file: !3147, line: 115, baseType: !3323, size: 192, offset: 448)
!3323 = !DIDerivedType(tag: DW_TAG_typedef, name: "file_handle_mapping_vector", scope: !3148, file: !3147, line: 92, baseType: !1263)
!3324 = !DIDerivedType(tag: DW_TAG_member, name: "fork_delegate", scope: !3146, file: !3147, line: 126, baseType: !3325, size: 64, offset: 640)
!3325 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "UniquePtr<base::LaunchOptions::ForkDelegate, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", scope: !20, file: !3326, line: 189, size: 64, flags: DIFlagTypePassByReference, elements: !3327, templateParams: !3408, identifier: "_ZTSN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEE")
!3326 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/UniquePtr.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3327 = !{!3328, !3401, !3415, !3422, !3427, !3432, !3435, !3438, !3447, !3455, !3459, !3462, !3463, !3467, !3470, !3478, !3481, !3484, !3485, !3486, !3487, !3490, !3491, !3494, !3498}
!3328 = !DIDerivedType(tag: DW_TAG_member, name: "mTuple", scope: !3325, file: !3326, line: 196, baseType: !3329, size: 64)
!3329 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "Pair<base::LaunchOptions::ForkDelegate *, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", scope: !20, file: !18, line: 136, size: 64, flags: DIFlagTypePassByReference, elements: !3330, templateParams: !3400, identifier: "_ZTSN7mozilla4PairIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEE")
!3330 = !{!3331, !3380, !3385, !3390, !3394, !3397}
!3331 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !3329, baseType: !3332, flags: DIFlagPrivate, extraData: i32 0)
!3332 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "PairHelper<base::LaunchOptions::ForkDelegate *, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate>, mozilla::detail::AsMember, mozilla::detail::AsBase>", scope: !19, file: !18, line: 61, size: 64, flags: DIFlagTypePassByValue, elements: !3333, templateParams: !3375, identifier: "_ZTSN7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EEE")
!3333 = !{!3334, !3350, !3351, !3356, !3363, !3367, !3371}
!3334 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !3332, baseType: !3335, flags: DIFlagPrivate, extraData: i32 0)
!3335 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "DefaultDelete<base::LaunchOptions::ForkDelegate>", scope: !20, file: !3326, line: 474, size: 8, flags: DIFlagTypePassByValue, elements: !3336, templateParams: !3348, identifier: "_ZTSN7mozilla13DefaultDeleteIN4base13LaunchOptions12ForkDelegateEEE")
!3336 = !{!3337, !3341}
!3337 = !DISubprogram(name: "DefaultDelete", scope: !3335, file: !3326, line: 476, type: !3338, isLocal: false, isDefinition: false, scopeLine: 476, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3338 = !DISubroutineType(types: !3339)
!3339 = !{null, !3340}
!3340 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3335, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3341 = !DISubprogram(name: "operator()", linkageName: "_ZNK7mozilla13DefaultDeleteIN4base13LaunchOptions12ForkDelegateEEclEPS3_", scope: !3335, file: !3326, line: 484, type: !3342, isLocal: false, isDefinition: false, scopeLine: 484, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3342 = !DISubroutineType(types: !3343)
!3343 = !{null, !3344, !3346}
!3344 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3345, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3345 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3335)
!3346 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3347, size: 64)
!3347 = !DICompositeType(tag: DW_TAG_structure_type, name: "ForkDelegate", scope: !3146, file: !3147, line: 119, flags: DIFlagFwdDecl, identifier: "_ZTSN4base13LaunchOptions12ForkDelegateE")
!3348 = !{!3349}
!3349 = !DITemplateTypeParameter(name: "T", type: !3347)
!3350 = !DIDerivedType(tag: DW_TAG_member, name: "mFirstA", scope: !3332, file: !18, line: 78, baseType: !3346, size: 64, flags: DIFlagPrivate)
!3351 = !DISubprogram(name: "first", linkageName: "_ZN7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EE5firstEv", scope: !3332, file: !18, line: 67, type: !3352, isLocal: false, isDefinition: false, scopeLine: 67, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3352 = !DISubroutineType(types: !3353)
!3353 = !{!3354, !3355}
!3354 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3346, size: 64)
!3355 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3332, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3356 = !DISubprogram(name: "first", linkageName: "_ZNK7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EE5firstEv", scope: !3332, file: !18, line: 68, type: !3357, isLocal: false, isDefinition: false, scopeLine: 68, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3357 = !DISubroutineType(types: !3358)
!3358 = !{!3359, !3361}
!3359 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3360, size: 64)
!3360 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3346)
!3361 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3362, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3362 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3332)
!3363 = !DISubprogram(name: "second", linkageName: "_ZN7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EE6secondEv", scope: !3332, file: !18, line: 69, type: !3364, isLocal: false, isDefinition: false, scopeLine: 69, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3364 = !DISubroutineType(types: !3365)
!3365 = !{!3366, !3355}
!3366 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3335, size: 64)
!3367 = !DISubprogram(name: "second", linkageName: "_ZNK7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EE6secondEv", scope: !3332, file: !18, line: 70, type: !3368, isLocal: false, isDefinition: false, scopeLine: 70, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3368 = !DISubroutineType(types: !3369)
!3369 = !{!3370, !3361}
!3370 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3345, size: 64)
!3371 = !DISubprogram(name: "swap", linkageName: "_ZN7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EE4swapERS9_", scope: !3332, file: !18, line: 72, type: !3372, isLocal: false, isDefinition: false, scopeLine: 72, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true)
!3372 = !DISubroutineType(types: !3373)
!3373 = !{null, !3355, !3374}
!3374 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3332, size: 64)
!3375 = !{!3376, !3377, !3378, !3379}
!3376 = !DITemplateTypeParameter(name: "A", type: !3346)
!3377 = !DITemplateTypeParameter(name: "B", type: !3335)
!3378 = !DITemplateValueParameter(type: !17, value: i32 1)
!3379 = !DITemplateValueParameter(type: !17, value: i32 0)
!3380 = !DISubprogram(name: "Pair", scope: !3329, file: !18, line: 144, type: !3381, isLocal: false, isDefinition: false, scopeLine: 144, flags: DIFlagPrototyped, isOptimized: true)
!3381 = !DISubroutineType(types: !3382)
!3382 = !{null, !3383, !3384}
!3383 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3329, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3384 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3329, size: 64)
!3385 = !DISubprogram(name: "Pair", scope: !3329, file: !18, line: 147, type: !3386, isLocal: false, isDefinition: false, scopeLine: 147, flags: DIFlagPrototyped, isOptimized: true)
!3386 = !DISubroutineType(types: !3387)
!3387 = !{null, !3383, !3388}
!3388 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3389, size: 64)
!3389 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3329)
!3390 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla4PairIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEaSEOS7_", scope: !3329, file: !18, line: 149, type: !3391, isLocal: false, isDefinition: false, scopeLine: 149, flags: DIFlagPrototyped, isOptimized: true)
!3391 = !DISubroutineType(types: !3392)
!3392 = !{!3393, !3383, !3384}
!3393 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3329, size: 64)
!3394 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla4PairIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEaSERKS7_", scope: !3329, file: !18, line: 158, type: !3395, isLocal: false, isDefinition: false, scopeLine: 158, flags: DIFlagPrototyped, isOptimized: true)
!3395 = !DISubroutineType(types: !3396)
!3396 = !{!3393, !3383, !3388}
!3397 = !DISubprogram(name: "swap", linkageName: "_ZN7mozilla4PairIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE4swapERS7_", scope: !3329, file: !18, line: 166, type: !3398, isLocal: false, isDefinition: false, scopeLine: 166, flags: DIFlagPrototyped, isOptimized: true)
!3398 = !DISubroutineType(types: !3399)
!3399 = !{null, !3383, !3393}
!3400 = !{!3376, !3377}
!3401 = !DISubprogram(name: "ptr", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE3ptrEv", scope: !3325, file: !3326, line: 198, type: !3402, isLocal: false, isDefinition: false, scopeLine: 198, flags: DIFlagPrototyped, isOptimized: true)
!3402 = !DISubroutineType(types: !3403)
!3403 = !{!3404, !3414}
!3404 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3405, size: 64)
!3405 = !DIDerivedType(tag: DW_TAG_typedef, name: "Pointer", scope: !3325, file: !3326, line: 193, baseType: !3406)
!3406 = !DIDerivedType(tag: DW_TAG_typedef, name: "Type", scope: !3407, file: !3326, line: 57, baseType: !3410)
!3407 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "PointerType<base::LaunchOptions::ForkDelegate, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", scope: !19, file: !3326, line: 55, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !3408, identifier: "_ZTSN7mozilla6detail11PointerTypeIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EEEE")
!3408 = !{!3349, !3409}
!3409 = !DITemplateTypeParameter(name: "D", type: !3335)
!3410 = !DIDerivedType(tag: DW_TAG_typedef, name: "Type", scope: !3411, file: !3326, line: 51, baseType: !3346)
!3411 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "PointerTypeImpl<base::LaunchOptions::ForkDelegate, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate>, false>", scope: !19, file: !3326, line: 50, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !3412, identifier: "_ZTSN7mozilla6detail15PointerTypeImplIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELb0EEE")
!3412 = !{!3349, !3409, !3413}
!3413 = !DITemplateValueParameter(type: !117, value: i8 0)
!3414 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3325, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3415 = !DISubprogram(name: "ptr", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE3ptrEv", scope: !3325, file: !3326, line: 199, type: !3416, isLocal: false, isDefinition: false, scopeLine: 199, flags: DIFlagPrototyped, isOptimized: true)
!3416 = !DISubroutineType(types: !3417)
!3417 = !{!3418, !3420}
!3418 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3419, size: 64)
!3419 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3405)
!3420 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3421, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3421 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3325)
!3422 = !DISubprogram(name: "del", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE3delEv", scope: !3325, file: !3326, line: 201, type: !3423, isLocal: false, isDefinition: false, scopeLine: 201, flags: DIFlagPrototyped, isOptimized: true)
!3423 = !DISubroutineType(types: !3424)
!3424 = !{!3425, !3414}
!3425 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3426, size: 64)
!3426 = !DIDerivedType(tag: DW_TAG_typedef, name: "DeleterType", scope: !3325, file: !3326, line: 192, baseType: !3335)
!3427 = !DISubprogram(name: "del", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE3delEv", scope: !3325, file: !3326, line: 202, type: !3428, isLocal: false, isDefinition: false, scopeLine: 202, flags: DIFlagPrototyped, isOptimized: true)
!3428 = !DISubroutineType(types: !3429)
!3429 = !{!3430, !3420}
!3430 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3431, size: 64)
!3431 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3426)
!3432 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 208, type: !3433, isLocal: false, isDefinition: false, scopeLine: 208, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3433 = !DISubroutineType(types: !3434)
!3434 = !{null, !3414}
!3435 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 216, type: !3436, isLocal: false, isDefinition: false, scopeLine: 216, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3436 = !DISubroutineType(types: !3437)
!3437 = !{null, !3414, !3405}
!3438 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 221, type: !3439, isLocal: false, isDefinition: false, scopeLine: 221, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3439 = !DISubroutineType(types: !3440)
!3440 = !{null, !3414, !3405, !3441}
!3441 = !DIDerivedType(tag: DW_TAG_typedef, name: "Type", scope: !3442, file: !25, line: 1232, baseType: !3370)
!3442 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "Conditional<false, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate>, const mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> &>", scope: !20, file: !25, line: 1231, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !3443, identifier: "_ZTSN7mozilla11ConditionalILb0ENS_13DefaultDeleteIN4base13LaunchOptions12ForkDelegateEEERKS5_EE")
!3443 = !{!3444, !3445, !3446}
!3444 = !DITemplateValueParameter(name: "Condition", type: !117, value: i8 0)
!3445 = !DITemplateTypeParameter(name: "A", type: !3335)
!3446 = !DITemplateTypeParameter(name: "B", type: !3370)
!3447 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 247, type: !3448, isLocal: false, isDefinition: false, scopeLine: 247, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3448 = !DISubroutineType(types: !3449)
!3449 = !{null, !3414, !3405, !3450}
!3450 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3451, size: 64)
!3451 = !DIDerivedType(tag: DW_TAG_typedef, name: "Type", scope: !3452, file: !25, line: 864, baseType: !3335)
!3452 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "RemoveReference<mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", scope: !20, file: !25, line: 863, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !3453, identifier: "_ZTSN7mozilla15RemoveReferenceINS_13DefaultDeleteIN4base13LaunchOptions12ForkDelegateEEEEE")
!3453 = !{!3454}
!3454 = !DITemplateTypeParameter(name: "T", type: !3335)
!3455 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 253, type: !3456, isLocal: false, isDefinition: false, scopeLine: 253, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3456 = !DISubroutineType(types: !3457)
!3457 = !{null, !3414, !3458}
!3458 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3325, size: 64)
!3459 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 258, type: !3460, isLocal: false, isDefinition: false, scopeLine: 258, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3460 = !DISubroutineType(types: !3461)
!3461 = !{null, !3414, !502}
!3462 = !DISubprogram(name: "~UniquePtr", scope: !3325, file: !3326, line: 274, type: !3433, isLocal: false, isDefinition: false, scopeLine: 274, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3463 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEaSEOS6_", scope: !3325, file: !3326, line: 276, type: !3464, isLocal: false, isDefinition: false, scopeLine: 276, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3464 = !DISubroutineType(types: !3465)
!3465 = !{!3466, !3414, !3458}
!3466 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3325, size: 64)
!3467 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEaSEDn", scope: !3325, file: !3326, line: 295, type: !3468, isLocal: false, isDefinition: false, scopeLine: 295, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3468 = !DISubroutineType(types: !3469)
!3469 = !{!3466, !3414, !502}
!3470 = !DISubprogram(name: "operator*", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEdeEv", scope: !3325, file: !3326, line: 300, type: !3471, isLocal: false, isDefinition: false, scopeLine: 300, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3471 = !DISubroutineType(types: !3472)
!3472 = !{!3473, !3420}
!3473 = !DIDerivedType(tag: DW_TAG_typedef, name: "Type", scope: !3474, file: !25, line: 894, baseType: !3477)
!3474 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "AddLvalueReferenceHelper<base::LaunchOptions::ForkDelegate, mozilla::detail::TIsNotVoid>", scope: !19, file: !25, line: 893, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !3475, identifier: "_ZTSN7mozilla6detail24AddLvalueReferenceHelperIN4base13LaunchOptions12ForkDelegateELNS0_8VoidnessE1EEE")
!3475 = !{!3349, !3476}
!3476 = !DITemplateValueParameter(name: "V", type: !24, value: i32 1)
!3477 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3347, size: 64)
!3478 = !DISubprogram(name: "operator->", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEptEv", scope: !3325, file: !3326, line: 301, type: !3479, isLocal: false, isDefinition: false, scopeLine: 301, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3479 = !DISubroutineType(types: !3480)
!3480 = !{!3405, !3420}
!3481 = !DISubprogram(name: "operator bool", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEcvbEv", scope: !3325, file: !3326, line: 306, type: !3482, isLocal: false, isDefinition: false, scopeLine: 306, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3482 = !DISubroutineType(types: !3483)
!3483 = !{!117, !3420}
!3484 = !DISubprogram(name: "get", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE3getEv", scope: !3325, file: !3326, line: 308, type: !3479, isLocal: false, isDefinition: false, scopeLine: 308, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3485 = !DISubprogram(name: "get_deleter", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE11get_deleterEv", scope: !3325, file: !3326, line: 310, type: !3423, isLocal: false, isDefinition: false, scopeLine: 310, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3486 = !DISubprogram(name: "get_deleter", linkageName: "_ZNK7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE11get_deleterEv", scope: !3325, file: !3326, line: 311, type: !3428, isLocal: false, isDefinition: false, scopeLine: 311, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3487 = !DISubprogram(name: "release", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE7releaseEv", scope: !3325, file: !3326, line: 313, type: !3488, isLocal: false, isDefinition: false, scopeLine: 313, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3488 = !DISubroutineType(types: !3489)
!3489 = !{!3405, !3414}
!3490 = !DISubprogram(name: "reset", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE5resetEPS3_", scope: !3325, file: !3326, line: 319, type: !3436, isLocal: false, isDefinition: false, scopeLine: 319, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3491 = !DISubprogram(name: "swap", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE4swapERS6_", scope: !3325, file: !3326, line: 327, type: !3492, isLocal: false, isDefinition: false, scopeLine: 327, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3492 = !DISubroutineType(types: !3493)
!3493 = !{null, !3414, !3466}
!3494 = !DISubprogram(name: "UniquePtr", scope: !3325, file: !3326, line: 329, type: !3495, isLocal: false, isDefinition: false, scopeLine: 329, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3495 = !DISubroutineType(types: !3496)
!3496 = !{null, !3414, !3497}
!3497 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3421, size: 64)
!3498 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEaSERKS6_", scope: !3325, file: !3326, line: 330, type: !3495, isLocal: false, isDefinition: false, scopeLine: 330, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3499 = !DILocalVariable(name: "process", scope: !2908, file: !1, line: 35, type: !3500)
!3500 = !DIDerivedType(tag: DW_TAG_typedef, name: "ProcessHandle", scope: !3148, file: !3501, line: 27, baseType: !3502)
!3501 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/ipc/chromium/src/base/process.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3502 = !DIDerivedType(tag: DW_TAG_typedef, name: "pid_t", file: !3503, line: 97, baseType: !3504)
!3503 = !DIFile(filename: "/usr/include/sys/types.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3504 = !DIDerivedType(tag: DW_TAG_typedef, name: "__pid_t", file: !46, line: 152, baseType: !93)
!3505 = !DILocalVariable(name: "ok", scope: !2908, file: !1, line: 36, type: !117)
!3506 = !DILocalVariable(name: "stream", scope: !2908, file: !1, line: 44, type: !3507)
!3507 = !DIDerivedType(tag: DW_TAG_typedef, name: "ScopedCloseFile", scope: !20, file: !3508, line: 80, baseType: !3509)
!3508 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/FileUtils.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3509 = distinct !DICompositeType(tag: DW_TAG_class_type, name: "Scoped<mozilla::ScopedCloseFileTraits>", scope: !20, file: !3510, line: 68, size: 64, flags: DIFlagTypePassByReference, elements: !3511, templateParams: !3570, identifier: "_ZTSN7mozilla6ScopedINS_21ScopedCloseFileTraitsEEE")
!3510 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/Scoped.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3511 = !{!3512, !3523, !3527, !3532, !3536, !3537, !3542, !3543, !3544, !3548, !3551, !3552, !3555, !3559, !3560, !3563, !3567}
!3512 = !DIDerivedType(tag: DW_TAG_member, name: "mValue", scope: !3509, file: !3510, line: 154, baseType: !3513, size: 64)
!3513 = !DIDerivedType(tag: DW_TAG_typedef, name: "Resource", scope: !3509, file: !3510, line: 70, baseType: !3514)
!3514 = !DIDerivedType(tag: DW_TAG_typedef, name: "type", scope: !3515, file: !3508, line: 72, baseType: !2354)
!3515 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "ScopedCloseFileTraits", scope: !20, file: !3508, line: 71, size: 8, flags: DIFlagTypePassByValue, elements: !3516, identifier: "_ZTSN7mozilla21ScopedCloseFileTraitsE")
!3516 = !{!3517, !3520}
!3517 = !DISubprogram(name: "empty", linkageName: "_ZN7mozilla21ScopedCloseFileTraits5emptyEv", scope: !3515, file: !3508, line: 73, type: !3518, isLocal: false, isDefinition: false, scopeLine: 73, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!3518 = !DISubroutineType(types: !3519)
!3519 = !{!3514}
!3520 = !DISubprogram(name: "release", linkageName: "_ZN7mozilla21ScopedCloseFileTraits7releaseEP8_IO_FILE", scope: !3515, file: !3508, line: 74, type: !3521, isLocal: false, isDefinition: false, scopeLine: 74, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!3521 = !DISubroutineType(types: !3522)
!3522 = !{null, !3514}
!3523 = !DISubprogram(name: "Scoped", scope: !3509, file: !3510, line: 72, type: !3524, isLocal: false, isDefinition: false, scopeLine: 72, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3524 = !DISubroutineType(types: !3525)
!3525 = !{null, !3526}
!3526 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3509, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3527 = !DISubprogram(name: "Scoped", scope: !3509, file: !3510, line: 77, type: !3528, isLocal: false, isDefinition: false, scopeLine: 77, flags: DIFlagPublic | DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3528 = !DISubroutineType(types: !3529)
!3529 = !{null, !3526, !3530}
!3530 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3531, size: 64)
!3531 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3513)
!3532 = !DISubprogram(name: "Scoped", scope: !3509, file: !3510, line: 83, type: !3533, isLocal: false, isDefinition: false, scopeLine: 83, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3533 = !DISubroutineType(types: !3534)
!3534 = !{null, !3526, !3535}
!3535 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3509, size: 64)
!3536 = !DISubprogram(name: "~Scoped", scope: !3509, file: !3510, line: 89, type: !3524, isLocal: false, isDefinition: false, scopeLine: 89, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3537 = !DISubprogram(name: "operator _IO_FILE *const &", linkageName: "_ZNK7mozilla6ScopedINS_21ScopedCloseFileTraitsEEcvRKP8_IO_FILEEv", scope: !3509, file: !3510, line: 92, type: !3538, isLocal: false, isDefinition: false, scopeLine: 92, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3538 = !DISubroutineType(types: !3539)
!3539 = !{!3530, !3540}
!3540 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3541, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3541 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !3509)
!3542 = !DISubprogram(name: "operator->", linkageName: "_ZNK7mozilla6ScopedINS_21ScopedCloseFileTraitsEEptEv", scope: !3509, file: !3510, line: 93, type: !3538, isLocal: false, isDefinition: false, scopeLine: 93, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3543 = !DISubprogram(name: "get", linkageName: "_ZNK7mozilla6ScopedINS_21ScopedCloseFileTraitsEE3getEv", scope: !3509, file: !3510, line: 94, type: !3538, isLocal: false, isDefinition: false, scopeLine: 94, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3544 = !DISubprogram(name: "rwget", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEE5rwgetEv", scope: !3509, file: !3510, line: 96, type: !3545, isLocal: false, isDefinition: false, scopeLine: 96, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3545 = !DISubroutineType(types: !3546)
!3546 = !{!3547, !3526}
!3547 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3513, size: 64)
!3548 = !DISubprogram(name: "forget", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEE6forgetEv", scope: !3509, file: !3510, line: 107, type: !3549, isLocal: false, isDefinition: false, scopeLine: 107, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3549 = !DISubroutineType(types: !3550)
!3550 = !{!3513, !3526}
!3551 = !DISubprogram(name: "dispose", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEE7disposeEv", scope: !3509, file: !3510, line: 118, type: !3524, isLocal: false, isDefinition: false, scopeLine: 118, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3552 = !DISubprogram(name: "operator==", linkageName: "_ZNK7mozilla6ScopedINS_21ScopedCloseFileTraitsEEeqERKP8_IO_FILE", scope: !3509, file: !3510, line: 123, type: !3553, isLocal: false, isDefinition: false, scopeLine: 123, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3553 = !DISubroutineType(types: !3554)
!3554 = !{!117, !3540, !3530}
!3555 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEEaSERKP8_IO_FILE", scope: !3509, file: !3510, line: 133, type: !3556, isLocal: false, isDefinition: false, scopeLine: 133, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3556 = !DISubroutineType(types: !3557)
!3557 = !{!3558, !3526, !3530}
!3558 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3509, size: 64)
!3559 = !DISubprogram(name: "reset", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEE5resetERKP8_IO_FILE", scope: !3509, file: !3510, line: 135, type: !3556, isLocal: false, isDefinition: false, scopeLine: 135, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3560 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEEaSEOS2_", scope: !3509, file: !3510, line: 142, type: !3561, isLocal: false, isDefinition: false, scopeLine: 142, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3561 = !DISubroutineType(types: !3562)
!3562 = !{!3558, !3526, !3535}
!3563 = !DISubprogram(name: "Scoped", scope: !3509, file: !3510, line: 150, type: !3564, isLocal: false, isDefinition: false, scopeLine: 150, flags: DIFlagExplicit | DIFlagPrototyped, isOptimized: true)
!3564 = !DISubroutineType(types: !3565)
!3565 = !{null, !3526, !3566}
!3566 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !3541, size: 64)
!3567 = !DISubprogram(name: "operator=", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEEaSERKS2_", scope: !3509, file: !3510, line: 151, type: !3568, isLocal: false, isDefinition: false, scopeLine: 151, flags: DIFlagPrototyped, isOptimized: true)
!3568 = !DISubroutineType(types: !3569)
!3569 = !{!3558, !3526, !3566}
!3570 = !{!3571}
!3571 = !DITemplateTypeParameter(name: "Traits", type: !3515)
!3572 = !DILocalVariable(name: "dist", scope: !2908, file: !1, line: 51, type: !3573)
!3573 = !DICompositeType(tag: DW_TAG_array_type, baseType: !1171, size: 2048, elements: !3574)
!3574 = !{!3575}
!3575 = !DISubrange(count: 256)
!3576 = !DILocalVariable(name: "desc", scope: !2908, file: !1, line: 51, type: !3573)
!3577 = !DILocalVariable(name: "release", scope: !2908, file: !1, line: 51, type: !3573)
!3578 = !DILocalVariable(name: "codename", scope: !2908, file: !1, line: 51, type: !3573)
!3579 = !DILocation(line: 19, column: 32, scope: !2908)
!3580 = !DILocation(line: 19, column: 58, scope: !2908)
!3581 = !DILocation(line: 20, column: 32, scope: !2908)
!3582 = !DILocation(line: 20, column: 54, scope: !2908)
!3583 = !DILocation(line: 21, column: 7, scope: !3584)
!3584 = distinct !DILexicalBlock(scope: !2908, file: !1, line: 21, column: 7)
!3585 = !DILocation(line: 21, column: 37, scope: !3584)
!3586 = !DILocation(line: 21, column: 7, scope: !2908)
!3587 = !DILocation(line: 23, column: 3, scope: !2908)
!3588 = !DILocation(line: 23, column: 7, scope: !2908)
!3589 = !DILocation(line: 24, column: 12, scope: !3590)
!3590 = distinct !DILexicalBlock(scope: !2908, file: !1, line: 24, column: 7)
!3591 = !DILocation(line: 24, column: 7, scope: !3590)
!3592 = !DILocation(line: 24, column: 20, scope: !3590)
!3593 = !DILocation(line: 24, column: 7, scope: !2908)
!3594 = !DILocation(line: 29, column: 3, scope: !2908)
!3595 = !DILocation(line: 29, column: 35, scope: !2908)
!3596 = !DILocation(line: 29, column: 36, scope: !2908)
!3597 = !DILocation(line: 29, column: 53, scope: !2908)
!3598 = !DILocation(line: 29, column: 28, scope: !2908)
!3599 = !DILocalVariable(name: "this", arg: 1, scope: !3600, type: !3604, flags: DIFlagArtificial | DIFlagObjectPointer)
!3600 = distinct !DISubprogram(name: "vector", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEC2ESt16initializer_listIS5_ERKS6_", scope: !2924, file: !49, line: 515, type: !2962, isLocal: false, isDefinition: true, scopeLine: 518, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !2961, retainedNodes: !3601)
!3601 = !{!3599, !3602, !3603}
!3602 = !DILocalVariable(name: "__l", arg: 2, scope: !3600, file: !49, line: 515, type: !2964)
!3603 = !DILocalVariable(name: "__a", arg: 3, scope: !3600, file: !49, line: 516, type: !2934)
!3604 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2924, size: 64)
!3605 = !DILocation(line: 0, scope: !3600, inlinedAt: !3606)
!3606 = distinct !DILocation(line: 29, column: 35, scope: !2908)
!3607 = !DILocation(line: 516, column: 29, scope: !3600, inlinedAt: !3606)
!3608 = !DILocalVariable(name: "this", arg: 1, scope: !3609, type: !3612, flags: DIFlagArtificial | DIFlagObjectPointer)
!3609 = distinct !DISubprogram(name: "_Vector_base", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EEC2ERKS6_", scope: !990, file: !49, line: 251, type: !1127, isLocal: false, isDefinition: true, scopeLine: 252, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1126, retainedNodes: !3610)
!3610 = !{!3608, !3611}
!3611 = !DILocalVariable(name: "__a", arg: 2, scope: !3609, file: !49, line: 251, type: !1129)
!3612 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !990, size: 64)
!3613 = !DILocation(line: 0, scope: !3609, inlinedAt: !3614)
!3614 = distinct !DILocation(line: 517, column: 9, scope: !3600, inlinedAt: !3606)
!3615 = !DILocation(line: 251, column: 42, scope: !3609, inlinedAt: !3614)
!3616 = !DILocalVariable(name: "this", arg: 1, scope: !3617, type: !3620, flags: DIFlagArtificial | DIFlagObjectPointer)
!3617 = distinct !DISubprogram(name: "_Vector_impl", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE12_Vector_implC2ERKS6_", scope: !993, file: !49, line: 99, type: !1097, isLocal: false, isDefinition: true, scopeLine: 101, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1096, retainedNodes: !3618)
!3618 = !{!3616, !3619}
!3619 = !DILocalVariable(name: "__a", arg: 2, scope: !3617, file: !49, line: 99, type: !1099)
!3620 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !993, size: 64)
!3621 = !DILocation(line: 0, scope: !3617, inlinedAt: !3622)
!3622 = distinct !DILocation(line: 252, column: 9, scope: !3609, inlinedAt: !3614)
!3623 = !DILocation(line: 99, column: 37, scope: !3617, inlinedAt: !3622)
!3624 = !DILocation(line: 100, column: 37, scope: !3617, inlinedAt: !3622)
!3625 = !DILocation(line: 515, column: 43, scope: !3600, inlinedAt: !3606)
!3626 = !DILocalVariable(name: "this", arg: 1, scope: !3627, type: !3629, flags: DIFlagArtificial | DIFlagObjectPointer)
!3627 = distinct !DISubprogram(name: "end", linkageName: "_ZNKSt16initializer_listINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE3endEv", scope: !2964, file: !1304, line: 79, type: !2984, isLocal: false, isDefinition: true, scopeLine: 79, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !2986, retainedNodes: !3628)
!3628 = !{!3626}
!3629 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !2982, size: 64)
!3630 = !DILocation(line: 0, scope: !3627, inlinedAt: !3631)
!3631 = distinct !DILocation(line: 519, column: 39, scope: !3632, inlinedAt: !3606)
!3632 = distinct !DILexicalBlock(scope: !3600, file: !49, line: 518, column: 7)
!3633 = !DILocation(line: 79, column: 45, scope: !3627, inlinedAt: !3631)
!3634 = !DILocation(line: 519, column: 2, scope: !3632, inlinedAt: !3606)
!3635 = !DILocalVariable(name: "this", arg: 1, scope: !3636, type: !1008, flags: DIFlagArtificial | DIFlagObjectPointer)
!3636 = distinct !DISubprogram(name: "~basic_string", linkageName: "_ZNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEED2Ev", scope: !31, file: !30, line: 656, type: !3637, isLocal: false, isDefinition: true, scopeLine: 657, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3640, retainedNodes: !3641)
!3637 = !DISubroutineType(types: !3638)
!3638 = !{null, !3639}
!3639 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !31, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3640 = !DISubprogram(name: "~basic_string", scope: !31, file: !30, line: 656, type: !3637, isLocal: false, isDefinition: false, scopeLine: 656, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true)
!3641 = !{!3635}
!3642 = !DILocation(line: 0, scope: !3636, inlinedAt: !3643)
!3643 = distinct !DILocation(line: 29, column: 35, scope: !2908)
!3644 = !DILocation(line: 657, column: 9, scope: !3645, inlinedAt: !3643)
!3645 = distinct !DILexicalBlock(scope: !3636, file: !30, line: 657, column: 7)
!3646 = !DILocation(line: 31, column: 3, scope: !2908)
!3647 = !DILocation(line: 31, column: 23, scope: !2908)
!3648 = !DILocalVariable(name: "this", arg: 1, scope: !3649, type: !3655, flags: DIFlagArtificial | DIFlagObjectPointer)
!3649 = distinct !DISubprogram(name: "LaunchOptions", linkageName: "_ZN4base13LaunchOptionsC2Ev", scope: !3146, file: !3147, line: 96, type: !3650, isLocal: false, isDefinition: true, scopeLine: 96, flags: DIFlagArtificial | DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3653, retainedNodes: !3654)
!3650 = !DISubroutineType(types: !3651)
!3651 = !{null, !3652}
!3652 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3146, size: 64, flags: DIFlagArtificial | DIFlagObjectPointer)
!3653 = !DISubprogram(name: "LaunchOptions", scope: !3146, type: !3650, isLocal: false, isDefinition: false, flags: DIFlagArtificial | DIFlagPrototyped, isOptimized: true)
!3654 = !{!3648}
!3655 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3146, size: 64)
!3656 = !DILocation(line: 0, scope: !3649, inlinedAt: !3657)
!3657 = distinct !DILocation(line: 31, column: 23, scope: !2908)
!3658 = !DILocation(line: 99, column: 8, scope: !3649, inlinedAt: !3657)
!3659 = !DILocalVariable(name: "this", arg: 1, scope: !3660, type: !3662, flags: DIFlagArtificial | DIFlagObjectPointer)
!3660 = distinct !DISubprogram(name: "map", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEEC2Ev", scope: !3153, file: !3154, line: 183, type: !3159, isLocal: false, isDefinition: true, scopeLine: 183, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3158, retainedNodes: !3661)
!3661 = !{!3659}
!3662 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3153, size: 64)
!3663 = !DILocation(line: 0, scope: !3660, inlinedAt: !3664)
!3664 = distinct !DILocation(line: 96, column: 8, scope: !3649, inlinedAt: !3657)
!3665 = !DILocalVariable(name: "this", arg: 1, scope: !3666, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!3666 = distinct !DISubprogram(name: "_Rb_tree", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EEC2Ev", scope: !303, file: !12, line: 929, type: !832, isLocal: false, isDefinition: true, scopeLine: 929, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !831, retainedNodes: !3667)
!3667 = !{!3665}
!3668 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !303, size: 64)
!3669 = !DILocation(line: 0, scope: !3666, inlinedAt: !3670)
!3670 = distinct !DILocation(line: 183, column: 7, scope: !3660, inlinedAt: !3664)
!3671 = !DILocalVariable(name: "this", arg: 1, scope: !3672, type: !3674, flags: DIFlagArtificial | DIFlagObjectPointer)
!3672 = distinct !DISubprogram(name: "_Rb_tree_impl", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE13_Rb_tree_implISC_Lb1EEC2Ev", scope: !306, file: !12, line: 699, type: !641, isLocal: false, isDefinition: true, scopeLine: 704, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !640, retainedNodes: !3673)
!3673 = !{!3671}
!3674 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !306, size: 64)
!3675 = !DILocation(line: 0, scope: !3672, inlinedAt: !3676)
!3676 = distinct !DILocation(line: 929, column: 7, scope: !3666, inlinedAt: !3670)
!3677 = !DILocation(line: 704, column: 4, scope: !3672, inlinedAt: !3676)
!3678 = !DILocalVariable(name: "this", arg: 1, scope: !3679, type: !3681, flags: DIFlagArtificial | DIFlagObjectPointer)
!3679 = distinct !DISubprogram(name: "_Rb_tree_header", linkageName: "_ZNSt15_Rb_tree_headerC2Ev", scope: !623, file: !12, line: 173, type: !628, isLocal: false, isDefinition: true, scopeLine: 174, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !627, retainedNodes: !3680)
!3680 = !{!3678}
!3681 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !623, size: 64)
!3682 = !DILocation(line: 0, scope: !3679, inlinedAt: !3683)
!3683 = distinct !DILocation(line: 699, column: 4, scope: !3672, inlinedAt: !3676)
!3684 = !DILocation(line: 175, column: 17, scope: !3685, inlinedAt: !3683)
!3685 = distinct !DILexicalBlock(scope: !3679, file: !12, line: 174, column: 5)
!3686 = !DILocation(line: 175, column: 26, scope: !3685, inlinedAt: !3683)
!3687 = !DILocalVariable(name: "this", arg: 1, scope: !3688, type: !3681, flags: DIFlagArtificial | DIFlagObjectPointer)
!3688 = distinct !DISubprogram(name: "_M_reset", linkageName: "_ZNSt15_Rb_tree_header8_M_resetEv", scope: !623, file: !12, line: 206, type: !628, isLocal: false, isDefinition: true, scopeLine: 207, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !639, retainedNodes: !3689)
!3689 = !{!3687}
!3690 = !DILocation(line: 0, scope: !3688, inlinedAt: !3691)
!3691 = distinct !DILocation(line: 176, column: 7, scope: !3685, inlinedAt: !3683)
!3692 = !DILocation(line: 208, column: 17, scope: !3688, inlinedAt: !3691)
!3693 = !DILocation(line: 208, column: 27, scope: !3688, inlinedAt: !3691)
!3694 = !DILocation(line: 209, column: 17, scope: !3688, inlinedAt: !3691)
!3695 = !DILocation(line: 209, column: 25, scope: !3688, inlinedAt: !3691)
!3696 = !DILocation(line: 210, column: 17, scope: !3688, inlinedAt: !3691)
!3697 = !DILocation(line: 210, column: 26, scope: !3688, inlinedAt: !3691)
!3698 = !DILocation(line: 211, column: 7, scope: !3688, inlinedAt: !3691)
!3699 = !DILocation(line: 211, column: 21, scope: !3688, inlinedAt: !3691)
!3700 = !DILocation(line: 96, column: 8, scope: !3649, inlinedAt: !3657)
!3701 = !DILocalVariable(name: "this", arg: 1, scope: !3702, type: !3705, flags: DIFlagArtificial | DIFlagObjectPointer)
!3702 = distinct !DISubprogram(name: "UniquePtr", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEC2EDn", scope: !3325, file: !3326, line: 258, type: !3460, isLocal: false, isDefinition: true, scopeLine: 258, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3459, retainedNodes: !3703)
!3703 = !{!3701, !3704}
!3704 = !DILocalVariable(arg: 2, scope: !3702, file: !3326, line: 258, type: !502)
!3705 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3325, size: 64)
!3706 = !DILocation(line: 0, scope: !3702, inlinedAt: !3707)
!3707 = distinct !DILocation(line: 126, column: 52, scope: !3649, inlinedAt: !3657)
!3708 = !DILocation(line: 258, column: 30, scope: !3702, inlinedAt: !3707)
!3709 = !DILocalVariable(name: "this", arg: 1, scope: !3710, type: !3722, flags: DIFlagArtificial | DIFlagObjectPointer)
!3710 = distinct !DISubprogram(name: "Pair<nullptr_t, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", linkageName: "_ZN7mozilla4PairIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEEC2IDnS6_EEOT_OT0_", scope: !3329, file: !18, line: 141, type: !3711, isLocal: false, isDefinition: true, scopeLine: 142, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !3716, declaration: !3715, retainedNodes: !3719)
!3711 = !DISubroutineType(types: !3712)
!3712 = !{null, !3383, !3713, !3714}
!3713 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !502, size: 64)
!3714 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !3335, size: 64)
!3715 = !DISubprogram(name: "Pair<nullptr_t, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", scope: !3329, file: !18, line: 141, type: !3711, isLocal: false, isDefinition: false, scopeLine: 141, flags: DIFlagPrototyped, isOptimized: true, templateParams: !3716)
!3716 = !{!3717, !3718}
!3717 = !DITemplateTypeParameter(name: "AArg", type: !502)
!3718 = !DITemplateTypeParameter(name: "BArg", type: !3335)
!3719 = !{!3709, !3720, !3721}
!3720 = !DILocalVariable(name: "aA", arg: 2, scope: !3710, file: !18, line: 141, type: !3713)
!3721 = !DILocalVariable(name: "aB", arg: 3, scope: !3710, file: !18, line: 141, type: !3714)
!3722 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3329, size: 64)
!3723 = !DILocation(line: 0, scope: !3710, inlinedAt: !3724)
!3724 = distinct !DILocation(line: 258, column: 34, scope: !3702, inlinedAt: !3707)
!3725 = !DILocation(line: 141, column: 26, scope: !3710, inlinedAt: !3724)
!3726 = !DILocalVariable(name: "this", arg: 1, scope: !3727, type: !3734, flags: DIFlagArtificial | DIFlagObjectPointer)
!3727 = distinct !DISubprogram(name: "PairHelper<nullptr_t, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", linkageName: "_ZN7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EEC2IDnS7_EEOT_OT0_", scope: !3332, file: !18, line: 64, type: !3728, isLocal: false, isDefinition: true, scopeLine: 65, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !3716, declaration: !3730, retainedNodes: !3731)
!3728 = !DISubroutineType(types: !3729)
!3729 = !{null, !3355, !3713, !3714}
!3730 = !DISubprogram(name: "PairHelper<nullptr_t, mozilla::DefaultDelete<base::LaunchOptions::ForkDelegate> >", scope: !3332, file: !18, line: 64, type: !3728, isLocal: false, isDefinition: false, scopeLine: 64, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true, templateParams: !3716)
!3731 = !{!3726, !3732, !3733}
!3732 = !DILocalVariable(name: "aA", arg: 2, scope: !3727, file: !18, line: 64, type: !3713)
!3733 = !DILocalVariable(name: "aB", arg: 3, scope: !3727, file: !18, line: 64, type: !3714)
!3734 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3332, size: 64)
!3735 = !DILocation(line: 0, scope: !3727, inlinedAt: !3736)
!3736 = distinct !DILocation(line: 142, column: 9, scope: !3710, inlinedAt: !3724)
!3737 = !DILocation(line: 65, column: 36, scope: !3727, inlinedAt: !3736)
!3738 = !DILocation(line: 32, column: 34, scope: !2908)
!3739 = !DILocation(line: 32, column: 35, scope: !2908)
!3740 = !DILocalVariable(name: "this", arg: 1, scope: !3741, type: !70, flags: DIFlagArtificial | DIFlagObjectPointer)
!3741 = distinct !DISubprogram(name: "pair<int &, int, true>", linkageName: "_ZNSt4pairIiiEC2IRiiLb1EEEOT_OT0_", scope: !71, file: !72, line: 341, type: !3742, isLocal: false, isDefinition: true, scopeLine: 342, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !3747, declaration: !3746, retainedNodes: !3749)
!3742 = !DISubroutineType(types: !3743)
!3743 = !{null, !100, !3744, !3745}
!3744 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !93, size: 64)
!3745 = !DIDerivedType(tag: DW_TAG_rvalue_reference_type, baseType: !93, size: 64)
!3746 = !DISubprogram(name: "pair<int &, int, true>", scope: !71, file: !72, line: 341, type: !3742, isLocal: false, isDefinition: false, scopeLine: 341, flags: DIFlagPrototyped, isOptimized: true, templateParams: !3747)
!3747 = !{!3748, !94, !658}
!3748 = !DITemplateTypeParameter(name: "_U1", type: !3744)
!3749 = !{!3740, !3750, !3751}
!3750 = !DILocalVariable(name: "__x", arg: 2, scope: !3741, file: !72, line: 341, type: !3744)
!3751 = !DILocalVariable(name: "__y", arg: 3, scope: !3741, file: !72, line: 341, type: !3745)
!3752 = !DILocation(line: 0, scope: !3741, inlinedAt: !3753)
!3753 = distinct !DILocation(line: 32, column: 34, scope: !2908)
!3754 = !DILocation(line: 341, column: 23, scope: !3741, inlinedAt: !3753)
!3755 = !DILocation(line: 342, column: 4, scope: !3741, inlinedAt: !3753)
!3756 = !DILocation(line: 342, column: 10, scope: !3741, inlinedAt: !3753)
!3757 = !DILocation(line: 342, column: 35, scope: !3741, inlinedAt: !3753)
!3758 = !DILocalVariable(name: "this", arg: 1, scope: !3759, type: !3762, flags: DIFlagArtificial | DIFlagObjectPointer)
!3759 = distinct !DISubprogram(name: "push_back", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE9push_backEOS1_", scope: !1263, file: !49, line: 1090, type: !1401, isLocal: false, isDefinition: true, scopeLine: 1091, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1400, retainedNodes: !3760)
!3760 = !{!3758, !3761}
!3761 = !DILocalVariable(name: "__x", arg: 2, scope: !3759, file: !49, line: 1090, type: !1403)
!3762 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1263, size: 64)
!3763 = !DILocation(line: 0, scope: !3759, inlinedAt: !3764)
!3764 = distinct !DILocation(line: 32, column: 24, scope: !2908)
!3765 = !DILocation(line: 1090, column: 30, scope: !3759, inlinedAt: !3764)
!3766 = !DILocation(line: 1091, column: 9, scope: !3759, inlinedAt: !3764)
!3767 = !DILocation(line: 32, column: 3, scope: !2908)
!3768 = !DILocation(line: 33, column: 16, scope: !2908)
!3769 = !DILocation(line: 35, column: 3, scope: !2908)
!3770 = !DILocation(line: 35, column: 23, scope: !2908)
!3771 = !DILocation(line: 36, column: 13, scope: !2908)
!3772 = !DILocation(line: 37, column: 9, scope: !2908)
!3773 = !DILocation(line: 37, column: 3, scope: !2908)
!3774 = !DILocation(line: 0, scope: !3775)
!3775 = distinct !DILexicalBlock(scope: !3776, file: !1, line: 38, column: 12)
!3776 = distinct !DILexicalBlock(scope: !2908, file: !1, line: 38, column: 7)
!3777 = !DILocation(line: 38, column: 7, scope: !2908)
!3778 = !DILocation(line: 40, column: 5, scope: !3775)
!3779 = !DILocation(line: 41, column: 5, scope: !3775)
!3780 = !DILocation(line: 44, column: 26, scope: !2908)
!3781 = !DILocation(line: 44, column: 19, scope: !2908)
!3782 = !DILocation(line: 45, column: 8, scope: !3783)
!3783 = distinct !DILexicalBlock(scope: !2908, file: !1, line: 45, column: 7)
!3784 = !DILocation(line: 45, column: 7, scope: !2908)
!3785 = !DILocation(line: 47, column: 11, scope: !3786)
!3786 = distinct !DILexicalBlock(scope: !3783, file: !1, line: 45, column: 16)
!3787 = !DILocation(line: 47, column: 5, scope: !3786)
!3788 = !DILocation(line: 48, column: 5, scope: !3786)
!3789 = !DILocation(line: 51, column: 3, scope: !2908)
!3790 = !DILocation(line: 51, column: 8, scope: !2908)
!3791 = !DILocation(line: 51, column: 19, scope: !2908)
!3792 = !DILocation(line: 51, column: 30, scope: !2908)
!3793 = !DILocation(line: 51, column: 44, scope: !2908)
!3794 = !DILocation(line: 52, column: 7, scope: !3795)
!3795 = distinct !DILexicalBlock(scope: !2908, file: !1, line: 52, column: 7)
!3796 = !DILocation(line: 57, column: 45, scope: !3795)
!3797 = !DILocation(line: 52, column: 7, scope: !2908)
!3798 = !DILocation(line: 62, column: 16, scope: !2908)
!3799 = !DILocation(line: 63, column: 16, scope: !2908)
!3800 = !DILocation(line: 64, column: 12, scope: !2908)
!3801 = !DILocation(line: 65, column: 13, scope: !2908)
!3802 = !DILocation(line: 66, column: 3, scope: !2908)
!3803 = !DILocation(line: 0, scope: !2908)
!3804 = !DILocation(line: 67, column: 1, scope: !2908)
!3805 = !DILocation(line: 0, scope: !3786)
!3806 = !DILocalVariable(name: "this", arg: 1, scope: !3807, type: !3809, flags: DIFlagArtificial | DIFlagObjectPointer)
!3807 = distinct !DISubprogram(name: "~Scoped", linkageName: "_ZN7mozilla6ScopedINS_21ScopedCloseFileTraitsEED2Ev", scope: !3509, file: !3510, line: 89, type: !3524, isLocal: false, isDefinition: true, scopeLine: 89, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3536, retainedNodes: !3808)
!3808 = !{!3806}
!3809 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3509, size: 64)
!3810 = !DILocation(line: 0, scope: !3807, inlinedAt: !3811)
!3811 = distinct !DILocation(line: 67, column: 1, scope: !2908)
!3812 = !DILocation(line: 89, column: 15, scope: !3813, inlinedAt: !3811)
!3813 = distinct !DILexicalBlock(scope: !3807, file: !3510, line: 89, column: 13)
!3814 = !DILocation(line: 0, scope: !3815)
!3815 = distinct !DILexicalBlock(scope: !3795, file: !1, line: 57, column: 51)
!3816 = !DILocation(line: 0, scope: !3817)
!3817 = distinct !DILexicalBlock(scope: !3590, file: !1, line: 24, column: 27)
!3818 = distinct !DISubprogram(name: "~LaunchOptions", linkageName: "_ZN4base13LaunchOptionsD2Ev", scope: !3146, file: !3147, line: 96, type: !3650, isLocal: false, isDefinition: true, scopeLine: 96, flags: DIFlagArtificial | DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3819, retainedNodes: !3820)
!3819 = !DISubprogram(name: "~LaunchOptions", scope: !3146, type: !3650, isLocal: false, isDefinition: false, flags: DIFlagArtificial | DIFlagPrototyped, isOptimized: true)
!3820 = !{!3821}
!3821 = !DILocalVariable(name: "this", arg: 1, scope: !3818, type: !3655, flags: DIFlagArtificial | DIFlagObjectPointer)
!3822 = !DILocation(line: 0, scope: !3818)
!3823 = !DILocation(line: 96, column: 8, scope: !3824)
!3824 = distinct !DILexicalBlock(scope: !3818, file: !3147, line: 96, column: 8)
!3825 = !DILocalVariable(name: "this", arg: 1, scope: !3826, type: !3762, flags: DIFlagArtificial | DIFlagObjectPointer)
!3826 = distinct !DISubprogram(name: "~vector", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EED2Ev", scope: !1263, file: !49, line: 565, type: !1267, isLocal: false, isDefinition: true, scopeLine: 566, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1305, retainedNodes: !3827)
!3827 = !{!3825}
!3828 = !DILocation(line: 0, scope: !3826, inlinedAt: !3829)
!3829 = distinct !DILocation(line: 96, column: 8, scope: !3824)
!3830 = !DILocation(line: 567, column: 22, scope: !3831, inlinedAt: !3829)
!3831 = distinct !DILexicalBlock(scope: !3826, file: !49, line: 566, column: 7)
!3832 = !DILocation(line: 570, column: 7, scope: !3831, inlinedAt: !3829)
!3833 = !DILocalVariable(name: "this", arg: 1, scope: !3834, type: !3662, flags: DIFlagArtificial | DIFlagObjectPointer)
!3834 = distinct !DISubprogram(name: "~map", linkageName: "_ZNSt3mapINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_St4lessIS5_ESaISt4pairIKS5_S5_EEED2Ev", scope: !3153, file: !3154, line: 300, type: !3159, isLocal: false, isDefinition: true, scopeLine: 300, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3193, retainedNodes: !3835)
!3835 = !{!3833}
!3836 = !DILocation(line: 0, scope: !3834, inlinedAt: !3837)
!3837 = distinct !DILocation(line: 96, column: 8, scope: !3824)
!3838 = !DILocation(line: 300, column: 22, scope: !3839, inlinedAt: !3837)
!3839 = distinct !DILexicalBlock(scope: !3834, file: !3154, line: 300, column: 22)
!3840 = !DILocation(line: 96, column: 8, scope: !3818)
!3841 = distinct !DISubprogram(name: "~vector", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev", scope: !2924, file: !49, line: 565, type: !2928, isLocal: false, isDefinition: true, scopeLine: 566, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !2989, retainedNodes: !3842)
!3842 = !{!3843}
!3843 = !DILocalVariable(name: "this", arg: 1, scope: !3841, type: !3604, flags: DIFlagArtificial | DIFlagObjectPointer)
!3844 = !DILocation(line: 0, scope: !3841)
!3845 = !DILocation(line: 567, column: 22, scope: !3846)
!3846 = distinct !DILexicalBlock(scope: !3841, file: !49, line: 566, column: 7)
!3847 = !DILocation(line: 567, column: 30, scope: !3846)
!3848 = !DILocation(line: 567, column: 54, scope: !3846)
!3849 = !DILocalVariable(name: "__first", arg: 1, scope: !3850, file: !3851, line: 203, type: !1008)
!3850 = distinct !DISubprogram(name: "_Destroy<std::__cxx11::basic_string<char> *, std::__cxx11::basic_string<char> >", linkageName: "_ZSt8_DestroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_EvT_S7_RSaIT0_E", scope: !13, file: !3851, line: 203, type: !3852, isLocal: false, isDefinition: true, scopeLine: 205, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !3857, retainedNodes: !3854)
!3851 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_construct.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3852 = !DISubroutineType(types: !3853)
!3853 = !{null, !1008, !1008, !1082}
!3854 = !{!3849, !3855, !3856}
!3855 = !DILocalVariable(name: "__last", arg: 2, scope: !3850, file: !3851, line: 203, type: !1008)
!3856 = !DILocalVariable(arg: 3, scope: !3850, file: !3851, line: 204, type: !1082)
!3857 = !{!3858, !602}
!3858 = !DITemplateTypeParameter(name: "_ForwardIterator", type: !1008)
!3859 = !DILocation(line: 203, column: 31, scope: !3850, inlinedAt: !3860)
!3860 = distinct !DILocation(line: 567, column: 2, scope: !3846)
!3861 = !DILocation(line: 203, column: 57, scope: !3850, inlinedAt: !3860)
!3862 = !DILocation(line: 204, column: 22, scope: !3850, inlinedAt: !3860)
!3863 = !DILocalVariable(name: "__first", arg: 1, scope: !3864, file: !3851, line: 127, type: !1008)
!3864 = distinct !DISubprogram(name: "_Destroy<std::__cxx11::basic_string<char> *>", linkageName: "_ZSt8_DestroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEvT_S7_", scope: !13, file: !3851, line: 127, type: !3865, isLocal: false, isDefinition: true, scopeLine: 128, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !3869, retainedNodes: !3867)
!3865 = !DISubroutineType(types: !3866)
!3866 = !{null, !1008, !1008}
!3867 = !{!3863, !3868}
!3868 = !DILocalVariable(name: "__last", arg: 2, scope: !3864, file: !3851, line: 127, type: !1008)
!3869 = !{!3858}
!3870 = !DILocation(line: 127, column: 31, scope: !3864, inlinedAt: !3871)
!3871 = distinct !DILocation(line: 206, column: 7, scope: !3850, inlinedAt: !3860)
!3872 = !DILocation(line: 127, column: 57, scope: !3864, inlinedAt: !3871)
!3873 = !DILocation(line: 136, column: 7, scope: !3864, inlinedAt: !3871)
!3874 = !DILocation(line: 570, column: 7, scope: !3846)
!3875 = !DILocation(line: 570, column: 7, scope: !3841)
!3876 = distinct !DISubprogram(name: "~UniquePtr", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEED2Ev", scope: !3325, file: !3326, line: 274, type: !3433, isLocal: false, isDefinition: true, scopeLine: 274, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3462, retainedNodes: !3877)
!3877 = !{!3878}
!3878 = !DILocalVariable(name: "this", arg: 1, scope: !3876, type: !3705, flags: DIFlagArtificial | DIFlagObjectPointer)
!3879 = !DILocation(line: 0, scope: !3876)
!3880 = !DILocalVariable(name: "this", arg: 1, scope: !3881, type: !3705, flags: DIFlagArtificial | DIFlagObjectPointer)
!3881 = distinct !DISubprogram(name: "reset", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE5resetEPS3_", scope: !3325, file: !3326, line: 319, type: !3436, isLocal: false, isDefinition: true, scopeLine: 319, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3490, retainedNodes: !3882)
!3882 = !{!3880, !3883, !3884}
!3883 = !DILocalVariable(name: "aPtr", arg: 2, scope: !3881, file: !3326, line: 319, type: !3405)
!3884 = !DILocalVariable(name: "old", scope: !3881, file: !3326, line: 320, type: !3405)
!3885 = !DILocation(line: 0, scope: !3881, inlinedAt: !3886)
!3886 = distinct !DILocation(line: 274, column: 18, scope: !3887)
!3887 = distinct !DILexicalBlock(scope: !3876, file: !3326, line: 274, column: 16)
!3888 = !DILocation(line: 319, column: 22, scope: !3881, inlinedAt: !3886)
!3889 = !DILocalVariable(name: "this", arg: 1, scope: !3890, type: !3705, flags: DIFlagArtificial | DIFlagObjectPointer)
!3890 = distinct !DISubprogram(name: "ptr", linkageName: "_ZN7mozilla9UniquePtrIN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS3_EEE3ptrEv", scope: !3325, file: !3326, line: 198, type: !3402, isLocal: false, isDefinition: true, scopeLine: 198, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3401, retainedNodes: !3891)
!3891 = !{!3889}
!3892 = !DILocation(line: 0, scope: !3890, inlinedAt: !3893)
!3893 = distinct !DILocation(line: 320, column: 19, scope: !3881, inlinedAt: !3886)
!3894 = !DILocalVariable(name: "this", arg: 1, scope: !3895, type: !3734, flags: DIFlagArtificial | DIFlagObjectPointer)
!3895 = distinct !DISubprogram(name: "first", linkageName: "_ZN7mozilla6detail10PairHelperIPN4base13LaunchOptions12ForkDelegateENS_13DefaultDeleteIS4_EELNS0_11StorageTypeE1ELS8_0EE5firstEv", scope: !3332, file: !18, line: 67, type: !3352, isLocal: false, isDefinition: true, scopeLine: 67, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3351, retainedNodes: !3896)
!3896 = !{!3894}
!3897 = !DILocation(line: 0, scope: !3895, inlinedAt: !3898)
!3898 = distinct !DILocation(line: 198, column: 34, scope: !3890, inlinedAt: !3893)
!3899 = !DILocation(line: 67, column: 23, scope: !3895, inlinedAt: !3898)
!3900 = !DILocation(line: 320, column: 19, scope: !3881, inlinedAt: !3886)
!3901 = !DILocation(line: 320, column: 13, scope: !3881, inlinedAt: !3886)
!3902 = !DILocation(line: 0, scope: !3890, inlinedAt: !3903)
!3903 = distinct !DILocation(line: 321, column: 5, scope: !3881, inlinedAt: !3886)
!3904 = !DILocation(line: 0, scope: !3895, inlinedAt: !3905)
!3905 = distinct !DILocation(line: 198, column: 34, scope: !3890, inlinedAt: !3903)
!3906 = !DILocation(line: 321, column: 11, scope: !3881, inlinedAt: !3886)
!3907 = !DILocation(line: 322, column: 13, scope: !3908, inlinedAt: !3886)
!3908 = distinct !DILexicalBlock(scope: !3881, file: !3326, line: 322, column: 9)
!3909 = !DILocation(line: 322, column: 9, scope: !3881, inlinedAt: !3886)
!3910 = !DILocalVariable(name: "aPtr", arg: 2, scope: !3911, file: !3326, line: 484, type: !3346)
!3911 = distinct !DISubprogram(name: "operator()", linkageName: "_ZNK7mozilla13DefaultDeleteIN4base13LaunchOptions12ForkDelegateEEclEPS3_", scope: !3335, file: !3326, line: 484, type: !3342, isLocal: false, isDefinition: true, scopeLine: 484, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3341, retainedNodes: !3912)
!3912 = !{!3913, !3910}
!3913 = !DILocalVariable(name: "this", arg: 1, scope: !3911, type: !3914, flags: DIFlagArtificial | DIFlagObjectPointer)
!3914 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !3345, size: 64)
!3915 = !DILocation(line: 484, column: 22, scope: !3911, inlinedAt: !3916)
!3916 = distinct !DILocation(line: 323, column: 7, scope: !3917, inlinedAt: !3886)
!3917 = distinct !DILexicalBlock(scope: !3908, file: !3326, line: 322, column: 25)
!3918 = !DILocation(line: 486, column: 5, scope: !3911, inlinedAt: !3916)
!3919 = !DILocation(line: 324, column: 5, scope: !3917, inlinedAt: !3886)
!3920 = !DILocation(line: 274, column: 34, scope: !3876)
!3921 = distinct !DISubprogram(name: "~_Vector_base", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EED2Ev", scope: !50, file: !49, line: 283, type: !267, isLocal: false, isDefinition: true, scopeLine: 284, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !290, retainedNodes: !3922)
!3922 = !{!3923}
!3923 = !DILocalVariable(name: "this", arg: 1, scope: !3921, type: !3924, flags: DIFlagArtificial | DIFlagObjectPointer)
!3924 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !50, size: 64)
!3925 = !DILocation(line: 0, scope: !3921)
!3926 = !DILocation(line: 285, column: 24, scope: !3927)
!3927 = distinct !DILexicalBlock(scope: !3921, file: !49, line: 284, column: 7)
!3928 = !DILocalVariable(name: "this", arg: 1, scope: !3929, type: !3924, flags: DIFlagArtificial | DIFlagObjectPointer)
!3929 = distinct !DISubprogram(name: "_M_deallocate", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE13_M_deallocateEPS1_m", scope: !50, file: !49, line: 300, type: !295, isLocal: false, isDefinition: true, scopeLine: 301, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !294, retainedNodes: !3930)
!3930 = !{!3928, !3931, !3932}
!3931 = !DILocalVariable(name: "__p", arg: 2, scope: !3929, file: !49, line: 300, type: !57)
!3932 = !DILocalVariable(name: "__n", arg: 3, scope: !3929, file: !49, line: 300, type: !175)
!3933 = !DILocation(line: 0, scope: !3929, inlinedAt: !3934)
!3934 = distinct !DILocation(line: 285, column: 2, scope: !3927)
!3935 = !DILocation(line: 300, column: 29, scope: !3929, inlinedAt: !3934)
!3936 = !DILocation(line: 303, column: 6, scope: !3937, inlinedAt: !3934)
!3937 = distinct !DILexicalBlock(scope: !3929, file: !49, line: 303, column: 6)
!3938 = !DILocation(line: 303, column: 6, scope: !3929, inlinedAt: !3934)
!3939 = !DILocalVariable(name: "__a", arg: 1, scope: !3940, file: !64, line: 461, type: !138)
!3940 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE10deallocateERS2_PS1_m", scope: !63, file: !64, line: 461, type: !204, isLocal: false, isDefinition: true, scopeLine: 462, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !203, retainedNodes: !3941)
!3941 = !{!3939, !3942, !3943}
!3942 = !DILocalVariable(name: "__p", arg: 2, scope: !3940, file: !64, line: 461, type: !69)
!3943 = !DILocalVariable(name: "__n", arg: 3, scope: !3940, file: !64, line: 461, type: !198)
!3944 = !DILocation(line: 461, column: 34, scope: !3940, inlinedAt: !3945)
!3945 = distinct !DILocation(line: 304, column: 4, scope: !3937, inlinedAt: !3934)
!3946 = !DILocation(line: 461, column: 47, scope: !3940, inlinedAt: !3945)
!3947 = !DILocalVariable(name: "this", arg: 1, scope: !3948, type: !3952, flags: DIFlagArtificial | DIFlagObjectPointer)
!3948 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE10deallocateEPS2_m", scope: !146, file: !147, line: 116, type: !181, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !180, retainedNodes: !3949)
!3949 = !{!3947, !3950, !3951}
!3950 = !DILocalVariable(name: "__p", arg: 2, scope: !3948, file: !147, line: 116, type: !162)
!3951 = !DILocalVariable(arg: 3, scope: !3948, file: !147, line: 116, type: !174)
!3952 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !146, size: 64)
!3953 = !DILocation(line: 0, scope: !3948, inlinedAt: !3954)
!3954 = distinct !DILocation(line: 462, column: 13, scope: !3940, inlinedAt: !3945)
!3955 = !DILocation(line: 116, column: 26, scope: !3948, inlinedAt: !3954)
!3956 = !DILocation(line: 125, column: 20, scope: !3948, inlinedAt: !3954)
!3957 = !DILocalVariable(name: "ptr", arg: 1, scope: !3958, file: !3959, line: 150, type: !506)
!3958 = distinct !DISubprogram(name: "operator delete", linkageName: "_ZdlPv", scope: !3959, file: !3959, line: 149, type: !1698, isLocal: false, isDefinition: true, scopeLine: 150, flags: DIFlagPrototyped, isOptimized: true, unit: !0, retainedNodes: !3960)
!3959 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/mozalloc.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!3960 = !{!3957}
!3961 = !DILocation(line: 150, column: 11, scope: !3958, inlinedAt: !3962)
!3962 = distinct !DILocation(line: 125, column: 2, scope: !3948, inlinedAt: !3954)
!3963 = !DILocation(line: 151, column: 10, scope: !3958, inlinedAt: !3962)
!3964 = !DILocation(line: 304, column: 4, scope: !3937, inlinedAt: !3934)
!3965 = !DILocation(line: 287, column: 7, scope: !3921)
!3966 = distinct !DISubprogram(name: "~_Rb_tree", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EED2Ev", scope: !303, file: !12, line: 964, type: !832, isLocal: false, isDefinition: true, scopeLine: 965, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !858, retainedNodes: !3967)
!3967 = !{!3968}
!3968 = !DILocalVariable(name: "this", arg: 1, scope: !3966, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!3969 = !DILocation(line: 0, scope: !3966)
!3970 = !DILocalVariable(name: "this", arg: 1, scope: !3971, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!3971 = distinct !DISubprogram(name: "_M_begin", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_beginEv", scope: !303, file: !12, line: 752, type: !676, isLocal: false, isDefinition: true, scopeLine: 753, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !696, retainedNodes: !3972)
!3972 = !{!3970}
!3973 = !DILocation(line: 0, scope: !3971, inlinedAt: !3974)
!3974 = distinct !DILocation(line: 965, column: 18, scope: !3975)
!3975 = distinct !DILexicalBlock(scope: !3966, file: !12, line: 965, column: 7)
!3976 = !DILocation(line: 753, column: 40, scope: !3971, inlinedAt: !3974)
!3977 = !DILocation(line: 753, column: 64, scope: !3971, inlinedAt: !3974)
!3978 = !DILocation(line: 965, column: 9, scope: !3975)
!3979 = !DILocation(line: 965, column: 31, scope: !3966)
!3980 = distinct !DISubprogram(name: "_M_erase", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_M_eraseEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 1867, type: !679, isLocal: false, isDefinition: true, scopeLine: 1868, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !822, retainedNodes: !3981)
!3981 = !{!3982, !3983, !3984}
!3982 = !DILocalVariable(name: "this", arg: 1, scope: !3980, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!3983 = !DILocalVariable(name: "__x", arg: 2, scope: !3980, file: !12, line: 906, type: !302)
!3984 = !DILocalVariable(name: "__y", scope: !3985, file: !12, line: 1873, type: !302)
!3985 = distinct !DILexicalBlock(scope: !3980, file: !12, line: 1871, column: 2)
!3986 = !DILocation(line: 0, scope: !3980)
!3987 = !DILocation(line: 906, column: 27, scope: !3980)
!3988 = !DILocation(line: 1870, column: 7, scope: !3980)
!3989 = !DILocation(line: 1870, column: 18, scope: !3980)
!3990 = !DILocalVariable(name: "__x", arg: 1, scope: !3991, file: !12, line: 787, type: !687)
!3991 = distinct !DISubprogram(name: "_S_right", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE8_S_rightEPSt18_Rb_tree_node_base", scope: !303, file: !12, line: 787, type: !716, isLocal: false, isDefinition: true, scopeLine: 788, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !721, retainedNodes: !3992)
!3992 = !{!3990}
!3993 = !DILocation(line: 787, column: 26, scope: !3991, inlinedAt: !3994)
!3994 = distinct !DILocation(line: 1872, column: 13, scope: !3985)
!3995 = !DILocation(line: 788, column: 45, scope: !3991, inlinedAt: !3994)
!3996 = !DILocation(line: 1872, column: 4, scope: !3985)
!3997 = !DILocalVariable(name: "__x", arg: 1, scope: !3998, file: !12, line: 779, type: !687)
!3998 = distinct !DISubprogram(name: "_S_left", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE7_S_leftEPSt18_Rb_tree_node_base", scope: !303, file: !12, line: 779, type: !716, isLocal: false, isDefinition: true, scopeLine: 780, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !715, retainedNodes: !3999)
!3999 = !{!3997}
!4000 = !DILocation(line: 779, column: 25, scope: !3998, inlinedAt: !4001)
!4001 = distinct !DILocation(line: 1873, column: 21, scope: !3985)
!4002 = !DILocation(line: 780, column: 45, scope: !3998, inlinedAt: !4001)
!4003 = !DILocation(line: 1873, column: 15, scope: !3985)
!4004 = !DILocation(line: 1874, column: 4, scope: !3985)
!4005 = distinct !{!4005, !3988, !4006}
!4006 = !DILocation(line: 1876, column: 2, scope: !3980)
!4007 = !DILocation(line: 1877, column: 5, scope: !3980)
!4008 = distinct !DISubprogram(name: "_M_drop_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE12_M_drop_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 667, type: !679, isLocal: false, isDefinition: true, scopeLine: 668, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !682, retainedNodes: !4009)
!4009 = !{!4010, !4011}
!4010 = !DILocalVariable(name: "this", arg: 1, scope: !4008, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!4011 = !DILocalVariable(name: "__p", arg: 2, scope: !4008, file: !12, line: 667, type: !302)
!4012 = !DILocation(line: 0, scope: !4008)
!4013 = !DILocation(line: 667, column: 31, scope: !4008)
!4014 = !DILocalVariable(name: "this", arg: 1, scope: !4015, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!4015 = distinct !DISubprogram(name: "_M_destroy_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE15_M_destroy_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 659, type: !679, isLocal: false, isDefinition: true, scopeLine: 660, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !681, retainedNodes: !4016)
!4016 = !{!4014, !4017}
!4017 = !DILocalVariable(name: "__p", arg: 2, scope: !4015, file: !12, line: 659, type: !302)
!4018 = !DILocation(line: 0, scope: !4015, inlinedAt: !4019)
!4019 = distinct !DILocation(line: 669, column: 2, scope: !4008)
!4020 = !DILocation(line: 659, column: 34, scope: !4015, inlinedAt: !4019)
!4021 = !DILocalVariable(name: "this", arg: 1, scope: !4022, type: !550, flags: DIFlagArtificial | DIFlagObjectPointer)
!4022 = distinct !DISubprogram(name: "_M_valptr", linkageName: "_ZNSt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES6_EE9_M_valptrEv", scope: !463, file: !12, line: 234, type: !519, isLocal: false, isDefinition: true, scopeLine: 235, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !518, retainedNodes: !4023)
!4023 = !{!4021}
!4024 = !DILocation(line: 0, scope: !4022, inlinedAt: !4025)
!4025 = distinct !DILocation(line: 661, column: 55, scope: !4015, inlinedAt: !4019)
!4026 = !DILocation(line: 235, column: 16, scope: !4022, inlinedAt: !4025)
!4027 = !DILocalVariable(name: "this", arg: 1, scope: !4028, type: !4030, flags: DIFlagArtificial | DIFlagObjectPointer)
!4028 = distinct !DISubprogram(name: "_M_ptr", linkageName: "_ZN9__gnu_cxx16__aligned_membufISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EE6_M_ptrEv", scope: !486, file: !487, line: 70, type: !513, isLocal: false, isDefinition: true, scopeLine: 71, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !512, retainedNodes: !4029)
!4029 = !{!4027}
!4030 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !486, size: 64)
!4031 = !DILocation(line: 0, scope: !4028, inlinedAt: !4032)
!4032 = distinct !DILocation(line: 235, column: 27, scope: !4022, inlinedAt: !4025)
!4033 = !DILocation(line: 71, column: 16, scope: !4028, inlinedAt: !4032)
!4034 = !DILocalVariable(name: "__a", arg: 1, scope: !4035, file: !64, line: 486, type: !4042)
!4035 = distinct !DISubprogram(name: "destroy<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE7destroyIS9_EEvRSB_PT_", scope: !4036, file: !64, line: 486, type: !4061, isLocal: false, isDefinition: true, scopeLine: 487, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4064, declaration: !4063, retainedNodes: !4066)
!4036 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "allocator_traits<std::allocator<std::_Rb_tree_node<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > > > >", scope: !13, file: !64, line: 384, size: 8, flags: DIFlagTypePassByValue, elements: !4037, templateParams: !4059, identifier: "_ZTSSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE")
!4037 = !{!4038, !4044, !4047, !4050, !4056}
!4038 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE8allocateERSB_m", scope: !4036, file: !64, line: 435, type: !4039, isLocal: false, isDefinition: false, scopeLine: 435, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!4039 = !DISubroutineType(types: !4040)
!4040 = !{!4041, !4042, !198}
!4041 = !DIDerivedType(tag: DW_TAG_typedef, name: "pointer", scope: !4036, file: !64, line: 392, baseType: !550)
!4042 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !4043, size: 64)
!4043 = !DIDerivedType(tag: DW_TAG_typedef, name: "allocator_type", scope: !4036, file: !64, line: 387, baseType: !530)
!4044 = !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE8allocateERSB_mPKv", scope: !4036, file: !64, line: 449, type: !4045, isLocal: false, isDefinition: false, scopeLine: 449, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!4045 = !DISubroutineType(types: !4046)
!4046 = !{!4041, !4042, !198, !202}
!4047 = !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE10deallocateERSB_PSA_m", scope: !4036, file: !64, line: 461, type: !4048, isLocal: false, isDefinition: false, scopeLine: 461, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!4048 = !DISubroutineType(types: !4049)
!4049 = !{null, !4042, !4041, !198}
!4050 = !DISubprogram(name: "max_size", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE8max_sizeERKSB_", scope: !4036, file: !64, line: 495, type: !4051, isLocal: false, isDefinition: false, scopeLine: 495, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!4051 = !DISubroutineType(types: !4052)
!4052 = !{!4053, !4054}
!4053 = !DIDerivedType(tag: DW_TAG_typedef, name: "size_type", scope: !4036, file: !64, line: 407, baseType: !175)
!4054 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !4055, size: 64)
!4055 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !4043)
!4056 = !DISubprogram(name: "select_on_container_copy_construction", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE37select_on_container_copy_constructionERKSB_", scope: !4036, file: !64, line: 504, type: !4057, isLocal: false, isDefinition: false, scopeLine: 504, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true)
!4057 = !DISubroutineType(types: !4058)
!4058 = !{!4043, !4054}
!4059 = !{!4060}
!4060 = !DITemplateTypeParameter(name: "_Alloc", type: !530)
!4061 = !DISubroutineType(types: !4062)
!4062 = !{null, !4042, !321}
!4063 = !DISubprogram(name: "destroy<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE7destroyIS9_EEvRSB_PT_", scope: !4036, file: !64, line: 486, type: !4061, isLocal: false, isDefinition: false, scopeLine: 486, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !4064)
!4064 = !{!4065}
!4065 = !DITemplateTypeParameter(name: "_Up", type: !322)
!4066 = !{!4034, !4067}
!4067 = !DILocalVariable(name: "__p", arg: 2, scope: !4035, file: !64, line: 486, type: !321)
!4068 = !DILocation(line: 486, column: 26, scope: !4035, inlinedAt: !4069)
!4069 = distinct !DILocation(line: 661, column: 2, scope: !4015, inlinedAt: !4019)
!4070 = !DILocation(line: 486, column: 36, scope: !4035, inlinedAt: !4069)
!4071 = !DILocalVariable(name: "this", arg: 1, scope: !4072, type: !4078, flags: DIFlagArtificial | DIFlagObjectPointer)
!4072 = distinct !DISubprogram(name: "destroy<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", linkageName: "_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE7destroyISA_EEvPT_", scope: !534, file: !147, line: 140, type: !4073, isLocal: false, isDefinition: true, scopeLine: 140, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4064, declaration: !4075, retainedNodes: !4076)
!4073 = !DISubroutineType(types: !4074)
!4074 = !{null, !539, !321}
!4075 = !DISubprogram(name: "destroy<std::pair<const std::__cxx11::basic_string<char>, std::__cxx11::basic_string<char> > >", linkageName: "_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE7destroyISA_EEvPT_", scope: !534, file: !147, line: 140, type: !4073, isLocal: false, isDefinition: false, scopeLine: 140, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true, templateParams: !4064)
!4076 = !{!4071, !4077}
!4077 = !DILocalVariable(name: "__p", arg: 2, scope: !4072, file: !147, line: 140, type: !321)
!4078 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !534, size: 64)
!4079 = !DILocation(line: 0, scope: !4072, inlinedAt: !4080)
!4080 = distinct !DILocation(line: 487, column: 8, scope: !4035, inlinedAt: !4069)
!4081 = !DILocation(line: 140, column: 15, scope: !4072, inlinedAt: !4080)
!4082 = !DILocation(line: 140, column: 28, scope: !4072, inlinedAt: !4080)
!4083 = !DILocalVariable(name: "this", arg: 1, scope: !4084, type: !3668, flags: DIFlagArtificial | DIFlagObjectPointer)
!4084 = distinct !DISubprogram(name: "_M_put_node", linkageName: "_ZNSt8_Rb_treeINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESt4pairIKS5_S5_ESt10_Select1stIS8_ESt4lessIS5_ESaIS8_EE11_M_put_nodeEPSt13_Rb_tree_nodeIS8_E", scope: !303, file: !12, line: 602, type: !679, isLocal: false, isDefinition: true, scopeLine: 603, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !678, retainedNodes: !4085)
!4085 = !{!4083, !4086}
!4086 = !DILocalVariable(name: "__p", arg: 2, scope: !4084, file: !12, line: 602, type: !302)
!4087 = !DILocation(line: 0, scope: !4084, inlinedAt: !4088)
!4088 = distinct !DILocation(line: 670, column: 2, scope: !4008)
!4089 = !DILocation(line: 602, column: 30, scope: !4084, inlinedAt: !4088)
!4090 = !DILocalVariable(name: "__a", arg: 1, scope: !4091, file: !64, line: 461, type: !4042)
!4091 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES7_EEEE10deallocateERSB_PSA_m", scope: !4036, file: !64, line: 461, type: !4048, isLocal: false, isDefinition: true, scopeLine: 462, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !4047, retainedNodes: !4092)
!4092 = !{!4090, !4093, !4094}
!4093 = !DILocalVariable(name: "__p", arg: 2, scope: !4091, file: !64, line: 461, type: !4041)
!4094 = !DILocalVariable(name: "__n", arg: 3, scope: !4091, file: !64, line: 461, type: !198)
!4095 = !DILocation(line: 461, column: 34, scope: !4091, inlinedAt: !4096)
!4096 = distinct !DILocation(line: 603, column: 9, scope: !4084, inlinedAt: !4088)
!4097 = !DILocation(line: 461, column: 47, scope: !4091, inlinedAt: !4096)
!4098 = !DILocation(line: 461, column: 62, scope: !4091, inlinedAt: !4096)
!4099 = !DILocalVariable(name: "this", arg: 1, scope: !4100, type: !4078, flags: DIFlagArtificial | DIFlagObjectPointer)
!4100 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt13_Rb_tree_nodeISt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES8_EEE10deallocateEPSB_m", scope: !534, file: !147, line: 116, type: !565, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !564, retainedNodes: !4101)
!4101 = !{!4099, !4102, !4103}
!4102 = !DILocalVariable(name: "__p", arg: 2, scope: !4100, file: !147, line: 116, type: !549)
!4103 = !DILocalVariable(arg: 3, scope: !4100, file: !147, line: 116, type: !174)
!4104 = !DILocation(line: 0, scope: !4100, inlinedAt: !4105)
!4105 = distinct !DILocation(line: 462, column: 13, scope: !4091, inlinedAt: !4096)
!4106 = !DILocation(line: 116, column: 26, scope: !4100, inlinedAt: !4105)
!4107 = !DILocation(line: 116, column: 40, scope: !4100, inlinedAt: !4105)
!4108 = !DILocation(line: 125, column: 20, scope: !4100, inlinedAt: !4105)
!4109 = !DILocation(line: 150, column: 11, scope: !3958, inlinedAt: !4110)
!4110 = distinct !DILocation(line: 125, column: 2, scope: !4100, inlinedAt: !4105)
!4111 = !DILocation(line: 151, column: 10, scope: !3958, inlinedAt: !4110)
!4112 = !DILocation(line: 671, column: 7, scope: !4008)
!4113 = distinct !DISubprogram(name: "~pair", linkageName: "_ZNSt4pairIKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEES5_ED2Ev", scope: !322, file: !72, line: 193, type: !4114, isLocal: false, isDefinition: true, scopeLine: 193, flags: DIFlagArtificial | DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !4116, retainedNodes: !4117)
!4114 = !DISubroutineType(types: !4115)
!4115 = !{null, !350}
!4116 = !DISubprogram(name: "~pair", scope: !322, type: !4114, isLocal: false, isDefinition: false, flags: DIFlagArtificial | DIFlagPrototyped, isOptimized: true)
!4117 = !{!4118}
!4118 = !DILocalVariable(name: "this", arg: 1, scope: !4113, type: !321, flags: DIFlagArtificial | DIFlagObjectPointer)
!4119 = !DILocation(line: 0, scope: !4113)
!4120 = !DILocation(line: 193, column: 56, scope: !4121)
!4121 = distinct !DILexicalBlock(scope: !4113, file: !72, line: 193, column: 56)
!4122 = !DILocation(line: 0, scope: !3636, inlinedAt: !4123)
!4123 = distinct !DILocation(line: 193, column: 56, scope: !4121)
!4124 = !DILocation(line: 657, column: 9, scope: !3645, inlinedAt: !4123)
!4125 = !DILocation(line: 0, scope: !3636, inlinedAt: !4126)
!4126 = distinct !DILocation(line: 193, column: 56, scope: !4121)
!4127 = !DILocation(line: 657, column: 9, scope: !3645, inlinedAt: !4126)
!4128 = !DILocation(line: 193, column: 56, scope: !4113)
!4129 = distinct !DISubprogram(name: "~_Vector_base", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EED2Ev", scope: !990, file: !49, line: 283, type: !1124, isLocal: false, isDefinition: true, scopeLine: 284, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1147, retainedNodes: !4130)
!4130 = !{!4131}
!4131 = !DILocalVariable(name: "this", arg: 1, scope: !4129, type: !3612, flags: DIFlagArtificial | DIFlagObjectPointer)
!4132 = !DILocation(line: 0, scope: !4129)
!4133 = !DILocation(line: 285, column: 24, scope: !4134)
!4134 = distinct !DILexicalBlock(scope: !4129, file: !49, line: 284, column: 7)
!4135 = !DILocalVariable(name: "this", arg: 1, scope: !4136, type: !3612, flags: DIFlagArtificial | DIFlagObjectPointer)
!4136 = distinct !DISubprogram(name: "_M_deallocate", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE13_M_deallocateEPS5_m", scope: !990, file: !49, line: 300, type: !1152, isLocal: false, isDefinition: true, scopeLine: 301, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1151, retainedNodes: !4137)
!4137 = !{!4135, !4138, !4139}
!4138 = !DILocalVariable(name: "__p", arg: 2, scope: !4136, file: !49, line: 300, type: !997)
!4139 = !DILocalVariable(name: "__n", arg: 3, scope: !4136, file: !49, line: 300, type: !175)
!4140 = !DILocation(line: 0, scope: !4136, inlinedAt: !4141)
!4141 = distinct !DILocation(line: 285, column: 2, scope: !4134)
!4142 = !DILocation(line: 300, column: 29, scope: !4136, inlinedAt: !4141)
!4143 = !DILocation(line: 303, column: 6, scope: !4144, inlinedAt: !4141)
!4144 = distinct !DILexicalBlock(scope: !4136, file: !49, line: 303, column: 6)
!4145 = !DILocation(line: 303, column: 6, scope: !4136, inlinedAt: !4141)
!4146 = !DILocalVariable(name: "__a", arg: 1, scope: !4147, file: !64, line: 461, type: !1009)
!4147 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE10deallocateERS6_PS5_m", scope: !1002, file: !64, line: 461, type: !1063, isLocal: false, isDefinition: true, scopeLine: 462, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1062, retainedNodes: !4148)
!4148 = !{!4146, !4149, !4150}
!4149 = !DILocalVariable(name: "__p", arg: 2, scope: !4147, file: !64, line: 461, type: !1007)
!4150 = !DILocalVariable(name: "__n", arg: 3, scope: !4147, file: !64, line: 461, type: !198)
!4151 = !DILocation(line: 461, column: 34, scope: !4147, inlinedAt: !4152)
!4152 = distinct !DILocation(line: 304, column: 4, scope: !4144, inlinedAt: !4141)
!4153 = !DILocation(line: 461, column: 47, scope: !4147, inlinedAt: !4152)
!4154 = !DILocalVariable(name: "this", arg: 1, scope: !4155, type: !4159, flags: DIFlagArtificial | DIFlagObjectPointer)
!4155 = distinct !DISubprogram(name: "deallocate", linkageName: "_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE10deallocateEPS6_m", scope: !1015, file: !147, line: 116, type: !1044, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1043, retainedNodes: !4156)
!4156 = !{!4154, !4157, !4158}
!4157 = !DILocalVariable(name: "__p", arg: 2, scope: !4155, file: !147, line: 116, type: !1030)
!4158 = !DILocalVariable(arg: 3, scope: !4155, file: !147, line: 116, type: !174)
!4159 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1015, size: 64)
!4160 = !DILocation(line: 0, scope: !4155, inlinedAt: !4161)
!4161 = distinct !DILocation(line: 462, column: 13, scope: !4147, inlinedAt: !4152)
!4162 = !DILocation(line: 116, column: 26, scope: !4155, inlinedAt: !4161)
!4163 = !DILocation(line: 125, column: 20, scope: !4155, inlinedAt: !4161)
!4164 = !DILocation(line: 150, column: 11, scope: !3958, inlinedAt: !4165)
!4165 = distinct !DILocation(line: 125, column: 2, scope: !4155, inlinedAt: !4161)
!4166 = !DILocation(line: 151, column: 10, scope: !3958, inlinedAt: !4165)
!4167 = !DILocation(line: 304, column: 4, scope: !4144, inlinedAt: !4141)
!4168 = !DILocation(line: 287, column: 7, scope: !4129)
!4169 = distinct !DISubprogram(name: "__destroy<std::__cxx11::basic_string<char> *>", linkageName: "_ZNSt12_Destroy_auxILb0EE9__destroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEEvT_S9_", scope: !4170, file: !3851, line: 105, type: !3865, isLocal: false, isDefinition: true, scopeLine: 106, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !3869, declaration: !4172, retainedNodes: !4173)
!4170 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "_Destroy_aux<false>", scope: !13, file: !3851, line: 101, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !4171, identifier: "_ZTSSt12_Destroy_auxILb0EE")
!4171 = !{!3413}
!4172 = !DISubprogram(name: "__destroy<std::__cxx11::basic_string<char> *>", linkageName: "_ZNSt12_Destroy_auxILb0EE9__destroyIPNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEEvT_S9_", scope: !4170, file: !3851, line: 105, type: !3865, isLocal: false, isDefinition: false, scopeLine: 105, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !3869)
!4173 = !{!4174, !4175}
!4174 = !DILocalVariable(name: "__first", arg: 1, scope: !4169, file: !3851, line: 105, type: !1008)
!4175 = !DILocalVariable(name: "__last", arg: 2, scope: !4169, file: !3851, line: 105, type: !1008)
!4176 = !DILocation(line: 105, column: 36, scope: !4169)
!4177 = !DILocation(line: 105, column: 62, scope: !4169)
!4178 = !DILocation(line: 107, column: 4, scope: !4169)
!4179 = !DILocation(line: 107, column: 19, scope: !4180)
!4180 = distinct !DILexicalBlock(scope: !4181, file: !3851, line: 107, column: 4)
!4181 = distinct !DILexicalBlock(scope: !4169, file: !3851, line: 107, column: 4)
!4182 = !DILocation(line: 107, column: 4, scope: !4181)
!4183 = !DILocalVariable(name: "__pointer", arg: 1, scope: !4184, file: !3851, line: 97, type: !1008)
!4184 = distinct !DISubprogram(name: "_Destroy<std::__cxx11::basic_string<char> >", linkageName: "_ZSt8_DestroyINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEvPT_", scope: !13, file: !3851, line: 97, type: !4185, isLocal: false, isDefinition: true, scopeLine: 98, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !601, retainedNodes: !4187)
!4185 = !DISubroutineType(types: !4186)
!4186 = !{null, !1008}
!4187 = !{!4183}
!4188 = !DILocation(line: 97, column: 19, scope: !4184, inlinedAt: !4189)
!4189 = distinct !DILocation(line: 108, column: 6, scope: !4180)
!4190 = !DILocation(line: 0, scope: !3636, inlinedAt: !4191)
!4191 = distinct !DILocation(line: 98, column: 19, scope: !4184, inlinedAt: !4189)
!4192 = !DILocation(line: 657, column: 9, scope: !3645, inlinedAt: !4191)
!4193 = !DILocation(line: 107, column: 30, scope: !4180)
!4194 = !DILocation(line: 107, column: 4, scope: !4180)
!4195 = distinct !{!4195, !4182, !4196}
!4196 = !DILocation(line: 108, column: 46, scope: !4181)
!4197 = !DILocation(line: 109, column: 2, scope: !4169)
!4198 = distinct !DISubprogram(name: "_M_range_initialize<const std::__cxx11::basic_string<char> *>", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_range_initializeIPKS5_EEvT_SB_St20forward_iterator_tag", scope: !2924, file: !49, line: 1462, type: !4199, isLocal: false, isDefinition: true, scopeLine: 1464, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4206, declaration: !4205, retainedNodes: !4208)
!4199 = !DISubroutineType(types: !4200)
!4200 = !{null, !2930, !1038, !1038, !4201}
!4201 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "forward_iterator_tag", scope: !13, file: !1478, line: 95, size: 8, flags: DIFlagTypePassByValue, elements: !4202, identifier: "_ZTSSt20forward_iterator_tag")
!4202 = !{!4203}
!4203 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !4201, baseType: !4204, extraData: i32 0)
!4204 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "input_iterator_tag", scope: !13, file: !1478, line: 89, size: 8, flags: DIFlagTypePassByValue, elements: !114, identifier: "_ZTSSt18input_iterator_tag")
!4205 = !DISubprogram(name: "_M_range_initialize<const std::__cxx11::basic_string<char> *>", linkageName: "_ZNSt6vectorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE19_M_range_initializeIPKS5_EEvT_SB_St20forward_iterator_tag", scope: !2924, file: !49, line: 1462, type: !4199, isLocal: false, isDefinition: false, scopeLine: 1462, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true, templateParams: !4206)
!4206 = !{!4207}
!4207 = !DITemplateTypeParameter(name: "_ForwardIterator", type: !1038)
!4208 = !{!4209, !4210, !4211, !4212, !4213}
!4209 = !DILocalVariable(name: "this", arg: 1, scope: !4198, type: !3604, flags: DIFlagArtificial | DIFlagObjectPointer)
!4210 = !DILocalVariable(name: "__first", arg: 2, scope: !4198, file: !49, line: 1462, type: !1038)
!4211 = !DILocalVariable(name: "__last", arg: 3, scope: !4198, file: !49, line: 1462, type: !1038)
!4212 = !DILocalVariable(arg: 4, scope: !4198, file: !49, line: 1463, type: !4201)
!4213 = !DILocalVariable(name: "__n", scope: !4198, file: !49, line: 1465, type: !4214)
!4214 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !1260)
!4215 = !DILocation(line: 0, scope: !4198)
!4216 = !DILocation(line: 1462, column: 39, scope: !4198)
!4217 = !DILocation(line: 1462, column: 65, scope: !4198)
!4218 = !DILocalVariable(name: "__first", arg: 1, scope: !4219, file: !4220, line: 138, type: !1038)
!4219 = distinct !DISubprogram(name: "distance<const std::__cxx11::basic_string<char> *>", linkageName: "_ZSt8distanceIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEENSt15iterator_traitsIT_E15difference_typeES9_S9_", scope: !13, file: !4220, line: 138, type: !4221, isLocal: false, isDefinition: true, scopeLine: 139, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4229, retainedNodes: !4227)
!4220 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_iterator_base_funcs.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!4221 = !DISubroutineType(types: !4222)
!4222 = !{!4223, !1038, !1038}
!4223 = !DIDerivedType(tag: DW_TAG_typedef, name: "difference_type", scope: !4224, file: !1478, line: 193, baseType: !1503)
!4224 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "iterator_traits<const std::__cxx11::basic_string<char> *>", scope: !13, file: !1478, line: 189, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !4225, identifier: "_ZTSSt15iterator_traitsIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE")
!4225 = !{!4226}
!4226 = !DITemplateTypeParameter(name: "_Iterator", type: !1038)
!4227 = !{!4218, !4228}
!4228 = !DILocalVariable(name: "__last", arg: 2, scope: !4219, file: !4220, line: 138, type: !1038)
!4229 = !{!4230}
!4230 = !DITemplateTypeParameter(name: "_InputIterator", type: !1038)
!4231 = !DILocation(line: 138, column: 29, scope: !4219, inlinedAt: !4232)
!4232 = distinct !DILocation(line: 1465, column: 26, scope: !4198)
!4233 = !DILocation(line: 138, column: 53, scope: !4219, inlinedAt: !4232)
!4234 = !DILocalVariable(name: "__first", arg: 1, scope: !4235, file: !4220, line: 98, type: !1038)
!4235 = distinct !DISubprogram(name: "__distance<const std::__cxx11::basic_string<char> *>", linkageName: "_ZSt10__distanceIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEENSt15iterator_traitsIT_E15difference_typeES9_S9_St26random_access_iterator_tag", scope: !13, file: !4220, line: 98, type: !4236, isLocal: false, isDefinition: true, scopeLine: 100, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4247, retainedNodes: !4244)
!4236 = !DISubroutineType(types: !4237)
!4237 = !{!4223, !1038, !1038, !4238}
!4238 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "random_access_iterator_tag", scope: !13, file: !1478, line: 103, size: 8, flags: DIFlagTypePassByValue, elements: !4239, identifier: "_ZTSSt26random_access_iterator_tag")
!4239 = !{!4240}
!4240 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !4238, baseType: !4241, extraData: i32 0)
!4241 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "bidirectional_iterator_tag", scope: !13, file: !1478, line: 99, size: 8, flags: DIFlagTypePassByValue, elements: !4242, identifier: "_ZTSSt26bidirectional_iterator_tag")
!4242 = !{!4243}
!4243 = !DIDerivedType(tag: DW_TAG_inheritance, scope: !4241, baseType: !4201, extraData: i32 0)
!4244 = !{!4234, !4245, !4246}
!4245 = !DILocalVariable(name: "__last", arg: 2, scope: !4235, file: !4220, line: 98, type: !1038)
!4246 = !DILocalVariable(arg: 3, scope: !4235, file: !4220, line: 99, type: !4238)
!4247 = !{!4248}
!4248 = !DITemplateTypeParameter(name: "_RandomAccessIterator", type: !1038)
!4249 = !DILocation(line: 98, column: 38, scope: !4235, inlinedAt: !4250)
!4250 = distinct !DILocation(line: 141, column: 14, scope: !4219, inlinedAt: !4232)
!4251 = !DILocation(line: 98, column: 69, scope: !4235, inlinedAt: !4250)
!4252 = !DILocation(line: 104, column: 21, scope: !4235, inlinedAt: !4250)
!4253 = !DILocation(line: 1465, column: 20, scope: !4198)
!4254 = !DILocation(line: 1466, column: 35, scope: !4198)
!4255 = !DILocation(line: 1466, column: 18, scope: !4198)
!4256 = !DILocation(line: 1466, column: 27, scope: !4198)
!4257 = !DILocation(line: 1467, column: 61, scope: !4198)
!4258 = !DILocation(line: 1467, column: 18, scope: !4198)
!4259 = !DILocation(line: 1467, column: 36, scope: !4198)
!4260 = !DILocalVariable(name: "__first", arg: 1, scope: !4261, file: !4262, line: 287, type: !1038)
!4261 = distinct !DISubprogram(name: "__uninitialized_copy_a<const std::__cxx11::basic_string<char> *, std::__cxx11::basic_string<char> *, std::__cxx11::basic_string<char> >", linkageName: "_ZSt22__uninitialized_copy_aIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS5_S5_ET0_T_SA_S9_RSaIT1_E", scope: !13, file: !4262, line: 287, type: !4263, isLocal: false, isDefinition: true, scopeLine: 289, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4269, retainedNodes: !4265)
!4262 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_uninitialized.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!4263 = !DISubroutineType(types: !4264)
!4264 = !{!1008, !1038, !1038, !1008, !1082}
!4265 = !{!4260, !4266, !4267, !4268}
!4266 = !DILocalVariable(name: "__last", arg: 2, scope: !4261, file: !4262, line: 287, type: !1038)
!4267 = !DILocalVariable(name: "__result", arg: 3, scope: !4261, file: !4262, line: 288, type: !1008)
!4268 = !DILocalVariable(arg: 4, scope: !4261, file: !4262, line: 288, type: !1082)
!4269 = !{!4230, !3858, !602}
!4270 = !DILocation(line: 287, column: 43, scope: !4261, inlinedAt: !4271)
!4271 = distinct !DILocation(line: 1469, column: 6, scope: !4198)
!4272 = !DILocation(line: 287, column: 67, scope: !4261, inlinedAt: !4271)
!4273 = !DILocation(line: 288, column: 24, scope: !4261, inlinedAt: !4271)
!4274 = !DILocalVariable(name: "__first", arg: 1, scope: !4275, file: !4262, line: 115, type: !1038)
!4275 = distinct !DISubprogram(name: "uninitialized_copy<const std::__cxx11::basic_string<char> *, std::__cxx11::basic_string<char> *>", linkageName: "_ZSt18uninitialized_copyIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS5_ET0_T_SA_S9_", scope: !13, file: !4262, line: 115, type: !4276, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4282, retainedNodes: !4278)
!4276 = !DISubroutineType(types: !4277)
!4277 = !{!1008, !1038, !1038, !1008}
!4278 = !{!4274, !4279, !4280, !4281}
!4279 = !DILocalVariable(name: "__last", arg: 2, scope: !4275, file: !4262, line: 115, type: !1038)
!4280 = !DILocalVariable(name: "__result", arg: 3, scope: !4275, file: !4262, line: 116, type: !1008)
!4281 = !DILocalVariable(name: "__assignable", scope: !4275, file: !4262, line: 128, type: !954)
!4282 = !{!4230, !3858}
!4283 = !DILocation(line: 115, column: 39, scope: !4275, inlinedAt: !4284)
!4284 = distinct !DILocation(line: 289, column: 14, scope: !4261, inlinedAt: !4271)
!4285 = !DILocation(line: 115, column: 63, scope: !4275, inlinedAt: !4284)
!4286 = !DILocation(line: 116, column: 27, scope: !4275, inlinedAt: !4284)
!4287 = !DILocation(line: 128, column: 18, scope: !4275, inlinedAt: !4284)
!4288 = !DILocation(line: 131, column: 14, scope: !4275, inlinedAt: !4284)
!4289 = !DILocation(line: 1468, column: 18, scope: !4198)
!4290 = !DILocation(line: 1468, column: 28, scope: !4198)
!4291 = !DILocation(line: 1472, column: 2, scope: !4198)
!4292 = distinct !DISubprogram(name: "_M_allocate", linkageName: "_ZNSt12_Vector_baseINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEESaIS5_EE11_M_allocateEm", scope: !990, file: !49, line: 293, type: !1149, isLocal: false, isDefinition: true, scopeLine: 294, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1148, retainedNodes: !4293)
!4293 = !{!4294, !4295}
!4294 = !DILocalVariable(name: "this", arg: 1, scope: !4292, type: !3612, flags: DIFlagArtificial | DIFlagObjectPointer)
!4295 = !DILocalVariable(name: "__n", arg: 2, scope: !4292, file: !49, line: 293, type: !175)
!4296 = !DILocation(line: 0, scope: !4292)
!4297 = !DILocation(line: 293, column: 26, scope: !4292)
!4298 = !DILocation(line: 296, column: 13, scope: !4292)
!4299 = !DILocation(line: 296, column: 9, scope: !4292)
!4300 = !DILocation(line: 296, column: 34, scope: !4292)
!4301 = !DILocation(line: 296, column: 20, scope: !4292)
!4302 = !DILocation(line: 296, column: 2, scope: !4292)
!4303 = distinct !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEEE8allocateERS6_m", scope: !1002, file: !64, line: 435, type: !1005, isLocal: false, isDefinition: true, scopeLine: 436, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1004, retainedNodes: !4304)
!4304 = !{!4305, !4306}
!4305 = !DILocalVariable(name: "__a", arg: 1, scope: !4303, file: !64, line: 435, type: !1009)
!4306 = !DILocalVariable(name: "__n", arg: 2, scope: !4303, file: !64, line: 435, type: !198)
!4307 = !DILocation(line: 435, column: 32, scope: !4303)
!4308 = !DILocation(line: 435, column: 47, scope: !4303)
!4309 = !DILocation(line: 436, column: 16, scope: !4303)
!4310 = !DILocation(line: 436, column: 20, scope: !4303)
!4311 = !DILocation(line: 436, column: 9, scope: !4303)
!4312 = distinct !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEE8allocateEmPKv", scope: !1015, file: !147, line: 99, type: !1041, isLocal: false, isDefinition: true, scopeLine: 100, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1040, retainedNodes: !4313)
!4313 = !{!4314, !4315, !4316}
!4314 = !DILocalVariable(name: "this", arg: 1, scope: !4312, type: !4159, flags: DIFlagArtificial | DIFlagObjectPointer)
!4315 = !DILocalVariable(name: "__n", arg: 2, scope: !4312, file: !147, line: 99, type: !174)
!4316 = !DILocalVariable(arg: 3, scope: !4312, file: !147, line: 99, type: !178)
!4317 = !DILocation(line: 0, scope: !4312)
!4318 = !DILocation(line: 99, column: 26, scope: !4312)
!4319 = !DILocation(line: 99, column: 43, scope: !4312)
!4320 = !DILocation(line: 101, column: 10, scope: !4321)
!4321 = distinct !DILexicalBlock(scope: !4312, file: !147, line: 101, column: 6)
!4322 = !DILocation(line: 101, column: 6, scope: !4312)
!4323 = !DILocation(line: 58, column: 3, scope: !4324, inlinedAt: !4326)
!4324 = distinct !DISubprogram(name: "__throw_bad_alloc", linkageName: "_ZSt17__throw_bad_allocv", scope: !13, file: !4325, line: 56, type: !1652, isLocal: false, isDefinition: true, scopeLine: 57, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true, unit: !0, retainedNodes: !114)
!4325 = !DIFile(filename: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/dist/include/mozilla/throw_gcc.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!4326 = distinct !DILocation(line: 102, column: 4, scope: !4321)
!4327 = !DILocation(line: 111, column: 46, scope: !4312)
!4328 = !DILocalVariable(name: "size", arg: 1, scope: !4329, file: !3959, line: 130, type: !1678)
!4329 = distinct !DISubprogram(name: "operator new", linkageName: "_Znwm", scope: !3959, file: !3959, line: 130, type: !1714, isLocal: false, isDefinition: true, scopeLine: 130, flags: DIFlagPrototyped, isOptimized: true, unit: !0, retainedNodes: !4330)
!4330 = !{!4328}
!4331 = !DILocation(line: 130, column: 25, scope: !4329, inlinedAt: !4332)
!4332 = distinct !DILocation(line: 111, column: 27, scope: !4312)
!4333 = !DILocation(line: 131, column: 10, scope: !4329, inlinedAt: !4332)
!4334 = !DILocation(line: 111, column: 9, scope: !4312)
!4335 = !DILocation(line: 111, column: 2, scope: !4312)
!4336 = distinct !DISubprogram(name: "__uninit_copy<const std::__cxx11::basic_string<char> *, std::__cxx11::basic_string<char> *>", linkageName: "_ZNSt20__uninitialized_copyILb0EE13__uninit_copyIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS7_EET0_T_SC_SB_", scope: !4337, file: !4262, line: 76, type: !4276, isLocal: false, isDefinition: true, scopeLine: 78, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4282, declaration: !4340, retainedNodes: !4341)
!4337 = distinct !DICompositeType(tag: DW_TAG_structure_type, name: "__uninitialized_copy<false>", scope: !13, file: !4262, line: 72, size: 8, flags: DIFlagTypePassByValue, elements: !114, templateParams: !4338, identifier: "_ZTSSt20__uninitialized_copyILb0EE")
!4338 = !{!4339}
!4339 = !DITemplateValueParameter(name: "_TrivialValueTypes", type: !117, value: i8 0)
!4340 = !DISubprogram(name: "__uninit_copy<const std::__cxx11::basic_string<char> *, std::__cxx11::basic_string<char> *>", linkageName: "_ZNSt20__uninitialized_copyILb0EE13__uninit_copyIPKNSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEPS7_EET0_T_SC_SB_", scope: !4337, file: !4262, line: 76, type: !4276, isLocal: false, isDefinition: false, scopeLine: 76, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !4282)
!4341 = !{!4342, !4343, !4344, !4345}
!4342 = !DILocalVariable(name: "__first", arg: 1, scope: !4336, file: !4262, line: 76, type: !1038)
!4343 = !DILocalVariable(name: "__last", arg: 2, scope: !4336, file: !4262, line: 76, type: !1038)
!4344 = !DILocalVariable(name: "__result", arg: 3, scope: !4336, file: !4262, line: 77, type: !1008)
!4345 = !DILocalVariable(name: "__cur", scope: !4336, file: !4262, line: 79, type: !1008)
!4346 = !DILocation(line: 76, column: 38, scope: !4336)
!4347 = !DILocation(line: 76, column: 62, scope: !4336)
!4348 = !DILocation(line: 77, column: 26, scope: !4336)
!4349 = !DILocation(line: 79, column: 21, scope: !4336)
!4350 = !DILocation(line: 82, column: 8, scope: !4351)
!4351 = distinct !DILexicalBlock(scope: !4352, file: !4262, line: 81, column: 6)
!4352 = distinct !DILexicalBlock(scope: !4336, file: !4262, line: 80, column: 4)
!4353 = !DILocation(line: 0, scope: !4354)
!4354 = distinct !DILexicalBlock(scope: !4355, file: !4262, line: 82, column: 8)
!4355 = distinct !DILexicalBlock(scope: !4351, file: !4262, line: 82, column: 8)
!4356 = !DILocation(line: 82, column: 23, scope: !4354)
!4357 = !DILocation(line: 82, column: 8, scope: !4355)
!4358 = !DILocalVariable(name: "__p", arg: 1, scope: !4359, file: !3851, line: 74, type: !1008)
!4359 = distinct !DISubprogram(name: "_Construct<std::__cxx11::basic_string<char>, const std::__cxx11::basic_string<char> &>", linkageName: "_ZSt10_ConstructINSt7__cxx1112basic_stringIcSt11char_traitsIcESaIcEEEJRKS5_EEvPT_DpOT0_", scope: !13, file: !3851, line: 74, type: !4360, isLocal: false, isDefinition: true, scopeLine: 75, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4364, retainedNodes: !4362)
!4360 = !DISubroutineType(types: !4361)
!4361 = !{null, !1008, !600}
!4362 = !{!4358, !4363}
!4363 = !DILocalVariable(name: "__args", arg: 2, scope: !4359, file: !3851, line: 74, type: !600)
!4364 = !{!4365, !4366}
!4365 = !DITemplateTypeParameter(name: "_T1", type: !31)
!4366 = !DITemplateValueParameter(tag: DW_TAG_GNU_template_parameter_pack, name: "_Args", value: !4367)
!4367 = !{!4368}
!4368 = !DITemplateTypeParameter(type: !600)
!4369 = !DILocation(line: 74, column: 21, scope: !4359, inlinedAt: !4370)
!4370 = distinct !DILocation(line: 83, column: 3, scope: !4354)
!4371 = !DILocation(line: 74, column: 37, scope: !4359, inlinedAt: !4370)
!4372 = !DILocation(line: 75, column: 38, scope: !4359, inlinedAt: !4370)
!4373 = !DILocation(line: 82, column: 34, scope: !4354)
!4374 = !DILocation(line: 82, column: 51, scope: !4354)
!4375 = !DILocation(line: 82, column: 8, scope: !4354)
!4376 = distinct !{!4376, !4357, !4377}
!4377 = !DILocation(line: 83, column: 53, scope: !4355)
!4378 = !DILocation(line: 84, column: 8, scope: !4351)
!4379 = distinct !DISubprogram(name: "emplace_back<std::pair<int, int> >", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE12emplace_backIJS1_EEEvDpOT_", scope: !1263, file: !4380, line: 98, type: !4381, isLocal: false, isDefinition: true, scopeLine: 99, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4384, declaration: !4383, retainedNodes: !4387)
!4380 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/vector.tcc", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!4381 = !DISubroutineType(types: !4382)
!4382 = !{null, !1269, !106}
!4383 = !DISubprogram(name: "emplace_back<std::pair<int, int> >", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE12emplace_backIJS1_EEEvDpOT_", scope: !1263, file: !49, line: 1099, type: !4381, isLocal: false, isDefinition: false, scopeLine: 1099, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true, templateParams: !4384)
!4384 = !{!4385}
!4385 = !DITemplateValueParameter(tag: DW_TAG_GNU_template_parameter_pack, name: "_Args", value: !4386)
!4386 = !{!232}
!4387 = !{!4388, !4389}
!4388 = !DILocalVariable(name: "this", arg: 1, scope: !4379, type: !3762, flags: DIFlagArtificial | DIFlagObjectPointer)
!4389 = !DILocalVariable(name: "__args", arg: 2, scope: !4379, file: !49, line: 1099, type: !106)
!4390 = !DILocation(line: 0, scope: !4379)
!4391 = !DILocation(line: 1099, column: 26, scope: !4379)
!4392 = !DILocation(line: 100, column: 20, scope: !4393)
!4393 = distinct !DILexicalBlock(scope: !4379, file: !4380, line: 100, column: 6)
!4394 = !DILocation(line: 100, column: 47, scope: !4393)
!4395 = !DILocation(line: 100, column: 30, scope: !4393)
!4396 = !DILocation(line: 100, column: 6, scope: !4379)
!4397 = !DILocalVariable(name: "__a", arg: 1, scope: !4398, file: !64, line: 474, type: !138)
!4398 = distinct !DISubprogram(name: "construct<std::pair<int, int>, std::pair<int, int> >", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE9constructIS1_JS1_EEEvRS2_PT_DpOT0_", scope: !63, file: !64, line: 474, type: !4399, isLocal: false, isDefinition: true, scopeLine: 475, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4402, declaration: !4401, retainedNodes: !4404)
!4399 = !DISubroutineType(types: !4400)
!4400 = !{null, !138, !70, !106}
!4401 = !DISubprogram(name: "construct<std::pair<int, int>, std::pair<int, int> >", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE9constructIS1_JS1_EEEvRS2_PT_DpOT0_", scope: !63, file: !64, line: 474, type: !4399, isLocal: false, isDefinition: false, scopeLine: 474, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !4402)
!4402 = !{!4403, !4385}
!4403 = !DITemplateTypeParameter(name: "_Up", type: !71)
!4404 = !{!4397, !4405, !4406}
!4405 = !DILocalVariable(name: "__p", arg: 2, scope: !4398, file: !64, line: 474, type: !70)
!4406 = !DILocalVariable(name: "__args", arg: 3, scope: !4398, file: !64, line: 474, type: !106)
!4407 = !DILocation(line: 474, column: 28, scope: !4398, inlinedAt: !4408)
!4408 = distinct !DILocation(line: 103, column: 6, scope: !4409)
!4409 = distinct !DILexicalBlock(scope: !4393, file: !4380, line: 101, column: 4)
!4410 = !DILocation(line: 474, column: 38, scope: !4398, inlinedAt: !4408)
!4411 = !DILocation(line: 474, column: 54, scope: !4398, inlinedAt: !4408)
!4412 = !DILocalVariable(name: "this", arg: 1, scope: !4413, type: !3952, flags: DIFlagArtificial | DIFlagObjectPointer)
!4413 = distinct !DISubprogram(name: "construct<std::pair<int, int>, std::pair<int, int> >", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE9constructIS2_JS2_EEEvPT_DpOT0_", scope: !146, file: !147, line: 135, type: !4414, isLocal: false, isDefinition: true, scopeLine: 136, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4402, declaration: !4416, retainedNodes: !4417)
!4414 = !DISubroutineType(types: !4415)
!4415 = !{null, !152, !70, !106}
!4416 = !DISubprogram(name: "construct<std::pair<int, int>, std::pair<int, int> >", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE9constructIS2_JS2_EEEvPT_DpOT0_", scope: !146, file: !147, line: 135, type: !4414, isLocal: false, isDefinition: false, scopeLine: 135, flags: DIFlagPublic | DIFlagPrototyped, isOptimized: true, templateParams: !4402)
!4417 = !{!4412, !4418, !4419}
!4418 = !DILocalVariable(name: "__p", arg: 2, scope: !4413, file: !147, line: 135, type: !70)
!4419 = !DILocalVariable(name: "__args", arg: 3, scope: !4413, file: !147, line: 135, type: !106)
!4420 = !DILocation(line: 0, scope: !4413, inlinedAt: !4421)
!4421 = distinct !DILocation(line: 475, column: 8, scope: !4398, inlinedAt: !4408)
!4422 = !DILocation(line: 135, column: 17, scope: !4413, inlinedAt: !4421)
!4423 = !DILocation(line: 135, column: 33, scope: !4413, inlinedAt: !4421)
!4424 = !DILocation(line: 136, column: 23, scope: !4413, inlinedAt: !4421)
!4425 = !DILocation(line: 105, column: 6, scope: !4409)
!4426 = !DILocation(line: 107, column: 4, scope: !4409)
!4427 = !DILocation(line: 109, column: 4, scope: !4393)
!4428 = !DILocation(line: 113, column: 7, scope: !4379)
!4429 = distinct !DISubprogram(name: "_M_realloc_insert<std::pair<int, int> >", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE17_M_realloc_insertIJS1_EEEvN9__gnu_cxx17__normal_iteratorIPS1_S3_EEDpOT_", scope: !1263, file: !4380, line: 414, type: !4430, isLocal: false, isDefinition: true, scopeLine: 421, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4384, declaration: !4432, retainedNodes: !4433)
!4430 = !DISubroutineType(types: !4431)
!4431 = !{null, !1269, !1262, !106}
!4432 = !DISubprogram(name: "_M_realloc_insert<std::pair<int, int> >", linkageName: "_ZNSt6vectorISt4pairIiiESaIS1_EE17_M_realloc_insertIJS1_EEEvN9__gnu_cxx17__normal_iteratorIPS1_S3_EEDpOT_", scope: !1263, file: !49, line: 1621, type: !4430, isLocal: false, isDefinition: false, scopeLine: 1621, flags: DIFlagProtected | DIFlagPrototyped, isOptimized: true, templateParams: !4384)
!4433 = !{!4434, !4435, !4436, !4437, !4438, !4439, !4440, !4441, !4442}
!4434 = !DILocalVariable(name: "this", arg: 1, scope: !4429, type: !3762, flags: DIFlagArtificial | DIFlagObjectPointer)
!4435 = !DILocalVariable(name: "__position", arg: 2, scope: !4429, file: !49, line: 1621, type: !1262)
!4436 = !DILocalVariable(name: "__args", arg: 3, scope: !4429, file: !49, line: 1621, type: !106)
!4437 = !DILocalVariable(name: "__len", scope: !4429, file: !4380, line: 422, type: !4214)
!4438 = !DILocalVariable(name: "__old_start", scope: !4429, file: !4380, line: 424, type: !1448)
!4439 = !DILocalVariable(name: "__old_finish", scope: !4429, file: !4380, line: 425, type: !1448)
!4440 = !DILocalVariable(name: "__elems_before", scope: !4429, file: !4380, line: 426, type: !4214)
!4441 = !DILocalVariable(name: "__new_start", scope: !4429, file: !4380, line: 427, type: !1448)
!4442 = !DILocalVariable(name: "__new_finish", scope: !4429, file: !4380, line: 428, type: !1448)
!4443 = !DILocation(line: 0, scope: !4429)
!4444 = !DILocation(line: 1621, column: 52, scope: !4429)
!4445 = !DILocation(line: 423, column: 2, scope: !4429)
!4446 = !DILocation(line: 422, column: 23, scope: !4429)
!4447 = !DILocation(line: 424, column: 35, scope: !4429)
!4448 = !DILocation(line: 424, column: 43, scope: !4429)
!4449 = !DILocation(line: 424, column: 15, scope: !4429)
!4450 = !DILocation(line: 425, column: 44, scope: !4429)
!4451 = !DILocation(line: 425, column: 15, scope: !4429)
!4452 = !DILocation(line: 426, column: 53, scope: !4429)
!4453 = !DILocation(line: 1621, column: 29, scope: !4429)
!4454 = !DILocalVariable(name: "__lhs", arg: 1, scope: !4455, file: !879, line: 966, type: !4458)
!4455 = distinct !DISubprogram(name: "operator-<std::pair<int, int> *, std::vector<std::pair<int, int>, std::allocator<std::pair<int, int> > > >", linkageName: "_ZN9__gnu_cxxmiIPSt4pairIiiESt6vectorIS2_SaIS2_EEEENS_17__normal_iteratorIT_T0_E15difference_typeERKSA_SD_", scope: !5, file: !879, line: 966, type: !4456, isLocal: false, isDefinition: true, scopeLine: 969, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !1516, retainedNodes: !4459)
!4456 = !DISubroutineType(types: !4457)
!4457 = !{!1501, !4458, !4458}
!4458 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1483, size: 64)
!4459 = !{!4454, !4460}
!4460 = !DILocalVariable(name: "__rhs", arg: 2, scope: !4455, file: !879, line: 967, type: !4458)
!4461 = !DILocation(line: 966, column: 63, scope: !4455, inlinedAt: !4462)
!4462 = distinct !DILocation(line: 426, column: 51, scope: !4429)
!4463 = !DILocation(line: 967, column: 56, scope: !4455, inlinedAt: !4462)
!4464 = !DILocation(line: 969, column: 27, scope: !4455, inlinedAt: !4462)
!4465 = !DILocation(line: 426, column: 23, scope: !4429)
!4466 = !DILocation(line: 427, column: 33, scope: !4429)
!4467 = !DILocation(line: 427, column: 15, scope: !4429)
!4468 = !DILocation(line: 428, column: 15, scope: !4429)
!4469 = !DILocation(line: 437, column: 20, scope: !4470)
!4470 = distinct !DILexicalBlock(scope: !4471, file: !4380, line: 430, column: 2)
!4471 = distinct !DILexicalBlock(scope: !4429, file: !4380, line: 429, column: 7)
!4472 = !DILocation(line: 474, column: 28, scope: !4398, inlinedAt: !4473)
!4473 = distinct !DILocation(line: 436, column: 4, scope: !4470)
!4474 = !DILocation(line: 474, column: 38, scope: !4398, inlinedAt: !4473)
!4475 = !DILocation(line: 474, column: 54, scope: !4398, inlinedAt: !4473)
!4476 = !DILocation(line: 0, scope: !4413, inlinedAt: !4477)
!4477 = distinct !DILocation(line: 475, column: 8, scope: !4398, inlinedAt: !4473)
!4478 = !DILocation(line: 135, column: 17, scope: !4413, inlinedAt: !4477)
!4479 = !DILocation(line: 135, column: 33, scope: !4413, inlinedAt: !4477)
!4480 = !DILocation(line: 136, column: 23, scope: !4413, inlinedAt: !4477)
!4481 = !DILocalVariable(name: "__first", arg: 1, scope: !4482, file: !4262, line: 305, type: !70)
!4482 = distinct !DISubprogram(name: "__uninitialized_move_if_noexcept_a<std::pair<int, int> *, std::pair<int, int> *, std::allocator<std::pair<int, int> > >", linkageName: "_ZSt34__uninitialized_move_if_noexcept_aIPSt4pairIiiES2_SaIS1_EET0_T_S5_S4_RT1_", scope: !13, file: !4262, line: 305, type: !4483, isLocal: false, isDefinition: true, scopeLine: 309, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4489, retainedNodes: !4485)
!4483 = !DISubroutineType(types: !4484)
!4484 = !{!70, !70, !70, !70, !223}
!4485 = !{!4481, !4486, !4487, !4488}
!4486 = !DILocalVariable(name: "__last", arg: 2, scope: !4482, file: !4262, line: 306, type: !70)
!4487 = !DILocalVariable(name: "__result", arg: 3, scope: !4482, file: !4262, line: 307, type: !70)
!4488 = !DILocalVariable(name: "__alloc", arg: 4, scope: !4482, file: !4262, line: 308, type: !223)
!4489 = !{!4490, !4491, !4492}
!4490 = !DITemplateTypeParameter(name: "_InputIterator", type: !70)
!4491 = !DITemplateTypeParameter(name: "_ForwardIterator", type: !70)
!4492 = !DITemplateTypeParameter(name: "_Allocator", type: !140)
!4493 = !DILocation(line: 305, column: 55, scope: !4482, inlinedAt: !4494)
!4494 = distinct !DILocation(line: 446, column: 8, scope: !4470)
!4495 = !DILocation(line: 306, column: 27, scope: !4482, inlinedAt: !4494)
!4496 = !DILocation(line: 307, column: 29, scope: !4482, inlinedAt: !4494)
!4497 = !DILocalVariable(name: "__first", arg: 1, scope: !4498, file: !4262, line: 287, type: !1518)
!4498 = distinct !DISubprogram(name: "__uninitialized_copy_a<std::move_iterator<std::pair<int, int> *>, std::pair<int, int> *, std::pair<int, int> >", linkageName: "_ZSt22__uninitialized_copy_aISt13move_iteratorIPSt4pairIiiEES3_S2_ET0_T_S6_S5_RSaIT1_E", scope: !13, file: !4262, line: 287, type: !4499, isLocal: false, isDefinition: true, scopeLine: 289, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4505, retainedNodes: !4501)
!4499 = !DISubroutineType(types: !4500)
!4500 = !{!70, !1518, !1518, !70, !223}
!4501 = !{!4497, !4502, !4503, !4504}
!4502 = !DILocalVariable(name: "__last", arg: 2, scope: !4498, file: !4262, line: 287, type: !1518)
!4503 = !DILocalVariable(name: "__result", arg: 3, scope: !4498, file: !4262, line: 288, type: !70)
!4504 = !DILocalVariable(arg: 4, scope: !4498, file: !4262, line: 288, type: !223)
!4505 = !{!4506, !4491, !187}
!4506 = !DITemplateTypeParameter(name: "_InputIterator", type: !1518)
!4507 = !DILocation(line: 287, column: 43, scope: !4498, inlinedAt: !4508)
!4508 = distinct !DILocation(line: 310, column: 14, scope: !4482, inlinedAt: !4494)
!4509 = !DILocation(line: 287, column: 67, scope: !4498, inlinedAt: !4508)
!4510 = !DILocation(line: 288, column: 24, scope: !4498, inlinedAt: !4508)
!4511 = !DILocalVariable(name: "__first", arg: 1, scope: !4512, file: !4262, line: 115, type: !1518)
!4512 = distinct !DISubprogram(name: "uninitialized_copy<std::move_iterator<std::pair<int, int> *>, std::pair<int, int> *>", linkageName: "_ZSt18uninitialized_copyISt13move_iteratorIPSt4pairIiiEES3_ET0_T_S6_S5_", scope: !13, file: !4262, line: 115, type: !4513, isLocal: false, isDefinition: true, scopeLine: 117, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4519, retainedNodes: !4515)
!4513 = !DISubroutineType(types: !4514)
!4514 = !{!70, !1518, !1518, !70}
!4515 = !{!4511, !4516, !4517, !4518}
!4516 = !DILocalVariable(name: "__last", arg: 2, scope: !4512, file: !4262, line: 115, type: !1518)
!4517 = !DILocalVariable(name: "__result", arg: 3, scope: !4512, file: !4262, line: 116, type: !70)
!4518 = !DILocalVariable(name: "__assignable", scope: !4512, file: !4262, line: 128, type: !954)
!4519 = !{!4506, !4491}
!4520 = !DILocation(line: 115, column: 39, scope: !4512, inlinedAt: !4521)
!4521 = distinct !DILocation(line: 289, column: 14, scope: !4498, inlinedAt: !4508)
!4522 = !DILocation(line: 115, column: 63, scope: !4512, inlinedAt: !4521)
!4523 = !DILocation(line: 116, column: 27, scope: !4512, inlinedAt: !4521)
!4524 = !DILocation(line: 128, column: 18, scope: !4512, inlinedAt: !4521)
!4525 = !DILocalVariable(name: "__result", arg: 3, scope: !4526, file: !4262, line: 77, type: !70)
!4526 = distinct !DISubprogram(name: "__uninit_copy<std::move_iterator<std::pair<int, int> *>, std::pair<int, int> *>", linkageName: "_ZNSt20__uninitialized_copyILb0EE13__uninit_copyISt13move_iteratorIPSt4pairIiiEES5_EET0_T_S8_S7_", scope: !4337, file: !4262, line: 76, type: !4513, isLocal: false, isDefinition: true, scopeLine: 78, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4519, declaration: !4527, retainedNodes: !4528)
!4527 = !DISubprogram(name: "__uninit_copy<std::move_iterator<std::pair<int, int> *>, std::pair<int, int> *>", linkageName: "_ZNSt20__uninitialized_copyILb0EE13__uninit_copyISt13move_iteratorIPSt4pairIiiEES5_EET0_T_S8_S7_", scope: !4337, file: !4262, line: 76, type: !4513, isLocal: false, isDefinition: false, scopeLine: 76, flags: DIFlagPrototyped | DIFlagStaticMember, isOptimized: true, templateParams: !4519)
!4528 = !{!4529, !4530, !4525, !4531}
!4529 = !DILocalVariable(name: "__first", arg: 1, scope: !4526, file: !4262, line: 76, type: !1518)
!4530 = !DILocalVariable(name: "__last", arg: 2, scope: !4526, file: !4262, line: 76, type: !1518)
!4531 = !DILocalVariable(name: "__cur", scope: !4526, file: !4262, line: 79, type: !70)
!4532 = !DILocation(line: 77, column: 26, scope: !4526, inlinedAt: !4533)
!4533 = distinct !DILocation(line: 131, column: 14, scope: !4512, inlinedAt: !4521)
!4534 = !DILocation(line: 79, column: 21, scope: !4526, inlinedAt: !4533)
!4535 = !DILocation(line: 82, column: 8, scope: !4536, inlinedAt: !4533)
!4536 = distinct !DILexicalBlock(scope: !4537, file: !4262, line: 81, column: 6)
!4537 = distinct !DILexicalBlock(scope: !4526, file: !4262, line: 80, column: 4)
!4538 = !DILocation(line: 0, scope: !4539, inlinedAt: !4533)
!4539 = distinct !DILexicalBlock(scope: !4540, file: !4262, line: 82, column: 8)
!4540 = distinct !DILexicalBlock(scope: !4536, file: !4262, line: 82, column: 8)
!4541 = !DILocation(line: 76, column: 38, scope: !4526, inlinedAt: !4533)
!4542 = !DILocation(line: 76, column: 62, scope: !4526, inlinedAt: !4533)
!4543 = !DILocalVariable(name: "__x", arg: 1, scope: !4544, file: !879, line: 1133, type: !4547)
!4544 = distinct !DISubprogram(name: "operator!=<std::pair<int, int> *>", linkageName: "_ZStneIPSt4pairIiiEEbRKSt13move_iteratorIT_ES7_", scope: !13, file: !879, line: 1133, type: !4545, isLocal: false, isDefinition: true, scopeLine: 1135, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !1480, retainedNodes: !4548)
!4545 = !DISubroutineType(types: !4546)
!4546 = !{!117, !4547, !4547}
!4547 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !1533, size: 64)
!4548 = !{!4543, !4549}
!4549 = !DILocalVariable(name: "__y", arg: 2, scope: !4544, file: !879, line: 1134, type: !4547)
!4550 = !DILocation(line: 1133, column: 48, scope: !4544, inlinedAt: !4551)
!4551 = distinct !DILocation(line: 82, column: 23, scope: !4539, inlinedAt: !4533)
!4552 = !DILocation(line: 1134, column: 41, scope: !4544, inlinedAt: !4551)
!4553 = !DILocalVariable(name: "__x", arg: 1, scope: !4554, file: !879, line: 1121, type: !4547)
!4554 = distinct !DISubprogram(name: "operator==<std::pair<int, int> *>", linkageName: "_ZSteqIPSt4pairIiiEEbRKSt13move_iteratorIT_ES7_", scope: !13, file: !879, line: 1121, type: !4545, isLocal: false, isDefinition: true, scopeLine: 1123, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !1480, retainedNodes: !4555)
!4555 = !{!4553, !4556}
!4556 = !DILocalVariable(name: "__y", arg: 2, scope: !4554, file: !879, line: 1122, type: !4547)
!4557 = !DILocation(line: 1121, column: 48, scope: !4554, inlinedAt: !4558)
!4558 = distinct !DILocation(line: 1135, column: 20, scope: !4544, inlinedAt: !4551)
!4559 = !DILocation(line: 1122, column: 41, scope: !4554, inlinedAt: !4558)
!4560 = !DILocation(line: 1123, column: 25, scope: !4554, inlinedAt: !4558)
!4561 = !DILocation(line: 82, column: 8, scope: !4540, inlinedAt: !4533)
!4562 = !DILocalVariable(name: "__p", arg: 1, scope: !4563, file: !3851, line: 74, type: !70)
!4563 = distinct !DISubprogram(name: "_Construct<std::pair<int, int>, std::pair<int, int> >", linkageName: "_ZSt10_ConstructISt4pairIiiEJS1_EEvPT_DpOT0_", scope: !13, file: !3851, line: 74, type: !4564, isLocal: false, isDefinition: true, scopeLine: 75, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4568, retainedNodes: !4566)
!4564 = !DISubroutineType(types: !4565)
!4565 = !{null, !70, !106}
!4566 = !{!4562, !4567}
!4567 = !DILocalVariable(name: "__args", arg: 2, scope: !4563, file: !3851, line: 74, type: !106)
!4568 = !{!4569, !4385}
!4569 = !DITemplateTypeParameter(name: "_T1", type: !71)
!4570 = !DILocation(line: 74, column: 21, scope: !4563, inlinedAt: !4571)
!4571 = distinct !DILocation(line: 83, column: 3, scope: !4539, inlinedAt: !4533)
!4572 = !DILocation(line: 74, column: 37, scope: !4563, inlinedAt: !4571)
!4573 = !DILocation(line: 75, column: 38, scope: !4563, inlinedAt: !4571)
!4574 = !DILocalVariable(name: "this", arg: 1, scope: !4575, type: !4577, flags: DIFlagArtificial | DIFlagObjectPointer)
!4575 = distinct !DISubprogram(name: "operator++", linkageName: "_ZNSt13move_iteratorIPSt4pairIiiEEppEv", scope: !1518, file: !879, line: 1054, type: !1547, isLocal: false, isDefinition: true, scopeLine: 1055, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1546, retainedNodes: !4576)
!4576 = !{!4574}
!4577 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1518, size: 64)
!4578 = !DILocation(line: 0, scope: !4575, inlinedAt: !4579)
!4579 = distinct !DILocation(line: 82, column: 34, scope: !4539, inlinedAt: !4533)
!4580 = !DILocation(line: 1056, column: 2, scope: !4575, inlinedAt: !4579)
!4581 = !DILocation(line: 82, column: 51, scope: !4539, inlinedAt: !4533)
!4582 = !DILocation(line: 82, column: 8, scope: !4539, inlinedAt: !4533)
!4583 = distinct !{!4583, !4584, !4585}
!4584 = !DILocation(line: 82, column: 8, scope: !4540)
!4585 = !DILocation(line: 83, column: 53, scope: !4540)
!4586 = !DILocation(line: 450, column: 4, scope: !4470)
!4587 = !DILocation(line: 305, column: 55, scope: !4482, inlinedAt: !4588)
!4588 = distinct !DILocation(line: 453, column: 8, scope: !4470)
!4589 = !DILocation(line: 306, column: 27, scope: !4482, inlinedAt: !4588)
!4590 = !DILocation(line: 307, column: 29, scope: !4482, inlinedAt: !4588)
!4591 = !DILocation(line: 287, column: 43, scope: !4498, inlinedAt: !4592)
!4592 = distinct !DILocation(line: 310, column: 14, scope: !4482, inlinedAt: !4588)
!4593 = !DILocation(line: 287, column: 67, scope: !4498, inlinedAt: !4592)
!4594 = !DILocation(line: 288, column: 24, scope: !4498, inlinedAt: !4592)
!4595 = !DILocation(line: 115, column: 39, scope: !4512, inlinedAt: !4596)
!4596 = distinct !DILocation(line: 289, column: 14, scope: !4498, inlinedAt: !4592)
!4597 = !DILocation(line: 115, column: 63, scope: !4512, inlinedAt: !4596)
!4598 = !DILocation(line: 116, column: 27, scope: !4512, inlinedAt: !4596)
!4599 = !DILocation(line: 128, column: 18, scope: !4512, inlinedAt: !4596)
!4600 = !DILocation(line: 77, column: 26, scope: !4526, inlinedAt: !4601)
!4601 = distinct !DILocation(line: 131, column: 14, scope: !4512, inlinedAt: !4596)
!4602 = !DILocation(line: 79, column: 21, scope: !4526, inlinedAt: !4601)
!4603 = !DILocation(line: 82, column: 8, scope: !4536, inlinedAt: !4601)
!4604 = !DILocation(line: 0, scope: !4539, inlinedAt: !4601)
!4605 = !DILocation(line: 76, column: 38, scope: !4526, inlinedAt: !4601)
!4606 = !DILocation(line: 76, column: 62, scope: !4526, inlinedAt: !4601)
!4607 = !DILocation(line: 1133, column: 48, scope: !4544, inlinedAt: !4608)
!4608 = distinct !DILocation(line: 82, column: 23, scope: !4539, inlinedAt: !4601)
!4609 = !DILocation(line: 1134, column: 41, scope: !4544, inlinedAt: !4608)
!4610 = !DILocation(line: 1121, column: 48, scope: !4554, inlinedAt: !4611)
!4611 = distinct !DILocation(line: 1135, column: 20, scope: !4544, inlinedAt: !4608)
!4612 = !DILocation(line: 1122, column: 41, scope: !4554, inlinedAt: !4611)
!4613 = !DILocation(line: 1123, column: 25, scope: !4554, inlinedAt: !4611)
!4614 = !DILocation(line: 82, column: 8, scope: !4540, inlinedAt: !4601)
!4615 = !DILocation(line: 74, column: 21, scope: !4563, inlinedAt: !4616)
!4616 = distinct !DILocation(line: 83, column: 3, scope: !4539, inlinedAt: !4601)
!4617 = !DILocation(line: 74, column: 37, scope: !4563, inlinedAt: !4616)
!4618 = !DILocation(line: 75, column: 38, scope: !4563, inlinedAt: !4616)
!4619 = !DILocation(line: 0, scope: !4575, inlinedAt: !4620)
!4620 = distinct !DILocation(line: 82, column: 34, scope: !4539, inlinedAt: !4601)
!4621 = !DILocation(line: 1056, column: 2, scope: !4575, inlinedAt: !4620)
!4622 = !DILocation(line: 82, column: 51, scope: !4539, inlinedAt: !4601)
!4623 = !DILocation(line: 82, column: 8, scope: !4539, inlinedAt: !4601)
!4624 = !DILocation(line: 470, column: 21, scope: !4429)
!4625 = !DILocation(line: 0, scope: !3929, inlinedAt: !4626)
!4626 = distinct !DILocation(line: 469, column: 7, scope: !4429)
!4627 = !DILocation(line: 300, column: 29, scope: !3929, inlinedAt: !4626)
!4628 = !DILocation(line: 303, column: 6, scope: !3937, inlinedAt: !4626)
!4629 = !DILocation(line: 303, column: 6, scope: !3929, inlinedAt: !4626)
!4630 = !DILocation(line: 461, column: 34, scope: !3940, inlinedAt: !4631)
!4631 = distinct !DILocation(line: 304, column: 4, scope: !3937, inlinedAt: !4626)
!4632 = !DILocation(line: 461, column: 47, scope: !3940, inlinedAt: !4631)
!4633 = !DILocation(line: 0, scope: !3948, inlinedAt: !4634)
!4634 = distinct !DILocation(line: 462, column: 13, scope: !3940, inlinedAt: !4631)
!4635 = !DILocation(line: 116, column: 26, scope: !3948, inlinedAt: !4634)
!4636 = !DILocation(line: 125, column: 20, scope: !3948, inlinedAt: !4634)
!4637 = !DILocation(line: 150, column: 11, scope: !3958, inlinedAt: !4638)
!4638 = distinct !DILocation(line: 125, column: 2, scope: !3948, inlinedAt: !4634)
!4639 = !DILocation(line: 151, column: 10, scope: !3958, inlinedAt: !4638)
!4640 = !DILocation(line: 304, column: 4, scope: !3937, inlinedAt: !4626)
!4641 = !DILocation(line: 471, column: 30, scope: !4429)
!4642 = !DILocation(line: 472, column: 31, scope: !4429)
!4643 = !DILocation(line: 473, column: 53, scope: !4429)
!4644 = !DILocation(line: 473, column: 39, scope: !4429)
!4645 = !DILocation(line: 474, column: 5, scope: !4429)
!4646 = distinct !DISubprogram(name: "_M_check_len", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE12_M_check_lenEmPKc", scope: !1263, file: !49, line: 1640, type: !1440, isLocal: false, isDefinition: true, scopeLine: 1641, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1439, retainedNodes: !4647)
!4647 = !{!4648, !4650, !4651, !4652}
!4648 = !DILocalVariable(name: "this", arg: 1, scope: !4646, type: !4649, flags: DIFlagArtificial | DIFlagObjectPointer)
!4649 = !DIDerivedType(tag: DW_TAG_pointer_type, baseType: !1289, size: 64)
!4650 = !DILocalVariable(name: "__n", arg: 2, scope: !4646, file: !49, line: 1640, type: !1260)
!4651 = !DILocalVariable(name: "__s", arg: 3, scope: !4646, file: !49, line: 1640, type: !1443)
!4652 = !DILocalVariable(name: "__len", scope: !4646, file: !49, line: 1645, type: !4214)
!4653 = !DILocation(line: 0, scope: !4646)
!4654 = !DILocation(line: 1640, column: 30, scope: !4646)
!4655 = !DILocation(line: 1640, column: 47, scope: !4646)
!4656 = !DILocalVariable(name: "this", arg: 1, scope: !4657, type: !4649, flags: DIFlagArtificial | DIFlagObjectPointer)
!4657 = distinct !DISubprogram(name: "size", linkageName: "_ZNKSt6vectorISt4pairIiiESaIS1_EE4sizeEv", scope: !1263, file: !49, line: 805, type: !1350, isLocal: false, isDefinition: true, scopeLine: 806, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !1349, retainedNodes: !4658)
!4658 = !{!4656}
!4659 = !DILocation(line: 0, scope: !4657, inlinedAt: !4660)
!4660 = distinct !DILocation(line: 1642, column: 19, scope: !4661)
!4661 = distinct !DILexicalBlock(scope: !4646, file: !49, line: 1642, column: 6)
!4662 = !DILocation(line: 806, column: 40, scope: !4657, inlinedAt: !4660)
!4663 = !DILocation(line: 806, column: 66, scope: !4657, inlinedAt: !4660)
!4664 = !DILocation(line: 806, column: 50, scope: !4657, inlinedAt: !4660)
!4665 = !DILocation(line: 1642, column: 17, scope: !4661)
!4666 = !DILocation(line: 1642, column: 26, scope: !4661)
!4667 = !DILocation(line: 1642, column: 6, scope: !4646)
!4668 = !DILocalVariable(name: "msg", arg: 1, scope: !4669, file: !4325, line: 93, type: !1443)
!4669 = distinct !DISubprogram(name: "__throw_length_error", linkageName: "_ZSt20__throw_length_errorPKc", scope: !13, file: !4325, line: 92, type: !2429, isLocal: false, isDefinition: true, scopeLine: 93, flags: DIFlagPrototyped | DIFlagNoReturn, isOptimized: true, unit: !0, retainedNodes: !4670)
!4670 = !{!4668}
!4671 = !DILocation(line: 93, column: 17, scope: !4669, inlinedAt: !4672)
!4672 = distinct !DILocation(line: 1643, column: 4, scope: !4661)
!4673 = !DILocation(line: 94, column: 3, scope: !4669, inlinedAt: !4672)
!4674 = !DILocation(line: 0, scope: !4657, inlinedAt: !4675)
!4675 = distinct !DILocation(line: 1645, column: 26, scope: !4646)
!4676 = !DILocation(line: 0, scope: !4657, inlinedAt: !4677)
!4677 = distinct !DILocation(line: 1645, column: 44, scope: !4646)
!4678 = !DILocation(line: 224, column: 15, scope: !4679, inlinedAt: !4692)
!4679 = distinct !DILexicalBlock(scope: !4681, file: !4680, line: 224, column: 11)
!4680 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/stl_algobase.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!4681 = distinct !DISubprogram(name: "max<unsigned long>", linkageName: "_ZSt3maxImERKT_S2_S2_", scope: !13, file: !4680, line: 219, type: !4682, isLocal: false, isDefinition: true, scopeLine: 220, flags: DIFlagPrototyped, isOptimized: true, unit: !0, templateParams: !4690, retainedNodes: !4686)
!4682 = !DISubroutineType(types: !4683)
!4683 = !{!4684, !4684, !4684}
!4684 = !DIDerivedType(tag: DW_TAG_reference_type, baseType: !4685, size: 64)
!4685 = !DIDerivedType(tag: DW_TAG_const_type, baseType: !177)
!4686 = !{!4687, !4689}
!4687 = !DILocalVariable(name: "__a", arg: 1, scope: !4681, file: !4688, line: 370, type: !4684)
!4688 = !DIFile(filename: "/usr/bin/../lib64/gcc/x86_64-pc-linux-gnu/8.2.1/../../../../include/c++/8.2.1/bits/algorithmfwd.h", directory: "/home/firefox/firefox/mozilla-unified/obj-opt-x86_64-pc-linux-gnu/widget")
!4689 = !DILocalVariable(name: "__b", arg: 2, scope: !4681, file: !4688, line: 370, type: !4684)
!4690 = !{!4691}
!4691 = !DITemplateTypeParameter(name: "_Tp", type: !177)
!4692 = distinct !DILocation(line: 1645, column: 35, scope: !4646)
!4693 = !DILocation(line: 1645, column: 35, scope: !4646)
!4694 = !DILocation(line: 1645, column: 33, scope: !4646)
!4695 = !DILocation(line: 1645, column: 18, scope: !4646)
!4696 = !DILocation(line: 0, scope: !4657, inlinedAt: !4697)
!4697 = distinct !DILocation(line: 1646, column: 18, scope: !4646)
!4698 = !DILocation(line: 1646, column: 16, scope: !4646)
!4699 = !DILocation(line: 1646, column: 34, scope: !4646)
!4700 = !DILocation(line: 1646, column: 25, scope: !4646)
!4701 = !DILocation(line: 1646, column: 2, scope: !4646)
!4702 = distinct !DISubprogram(name: "_M_allocate", linkageName: "_ZNSt12_Vector_baseISt4pairIiiESaIS1_EE11_M_allocateEm", scope: !50, file: !49, line: 293, type: !292, isLocal: false, isDefinition: true, scopeLine: 294, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !291, retainedNodes: !4703)
!4703 = !{!4704, !4705}
!4704 = !DILocalVariable(name: "this", arg: 1, scope: !4702, type: !3924, flags: DIFlagArtificial | DIFlagObjectPointer)
!4705 = !DILocalVariable(name: "__n", arg: 2, scope: !4702, file: !49, line: 293, type: !175)
!4706 = !DILocation(line: 0, scope: !4702)
!4707 = !DILocation(line: 293, column: 26, scope: !4702)
!4708 = !DILocation(line: 296, column: 13, scope: !4702)
!4709 = !DILocation(line: 296, column: 9, scope: !4702)
!4710 = !DILocation(line: 296, column: 34, scope: !4702)
!4711 = !DILocation(line: 296, column: 20, scope: !4702)
!4712 = !DILocation(line: 296, column: 2, scope: !4702)
!4713 = distinct !DISubprogram(name: "allocate", linkageName: "_ZNSt16allocator_traitsISaISt4pairIiiEEE8allocateERS2_m", scope: !63, file: !64, line: 435, type: !67, isLocal: false, isDefinition: true, scopeLine: 436, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !66, retainedNodes: !4714)
!4714 = !{!4715, !4716}
!4715 = !DILocalVariable(name: "__a", arg: 1, scope: !4713, file: !64, line: 435, type: !138)
!4716 = !DILocalVariable(name: "__n", arg: 2, scope: !4713, file: !64, line: 435, type: !198)
!4717 = !DILocation(line: 435, column: 32, scope: !4713)
!4718 = !DILocation(line: 435, column: 47, scope: !4713)
!4719 = !DILocation(line: 436, column: 16, scope: !4713)
!4720 = !DILocation(line: 436, column: 20, scope: !4713)
!4721 = !DILocation(line: 436, column: 9, scope: !4713)
!4722 = distinct !DISubprogram(name: "allocate", linkageName: "_ZN9__gnu_cxx13new_allocatorISt4pairIiiEE8allocateEmPKv", scope: !146, file: !147, line: 99, type: !172, isLocal: false, isDefinition: true, scopeLine: 100, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !171, retainedNodes: !4723)
!4723 = !{!4724, !4725, !4726}
!4724 = !DILocalVariable(name: "this", arg: 1, scope: !4722, type: !3952, flags: DIFlagArtificial | DIFlagObjectPointer)
!4725 = !DILocalVariable(name: "__n", arg: 2, scope: !4722, file: !147, line: 99, type: !174)
!4726 = !DILocalVariable(arg: 3, scope: !4722, file: !147, line: 99, type: !178)
!4727 = !DILocation(line: 0, scope: !4722)
!4728 = !DILocation(line: 99, column: 26, scope: !4722)
!4729 = !DILocation(line: 99, column: 43, scope: !4722)
!4730 = !DILocation(line: 101, column: 10, scope: !4731)
!4731 = distinct !DILexicalBlock(scope: !4722, file: !147, line: 101, column: 6)
!4732 = !DILocation(line: 101, column: 6, scope: !4722)
!4733 = !DILocation(line: 58, column: 3, scope: !4324, inlinedAt: !4734)
!4734 = distinct !DILocation(line: 102, column: 4, scope: !4731)
!4735 = !DILocation(line: 111, column: 46, scope: !4722)
!4736 = !DILocation(line: 130, column: 25, scope: !4329, inlinedAt: !4737)
!4737 = distinct !DILocation(line: 111, column: 27, scope: !4722)
!4738 = !DILocation(line: 131, column: 10, scope: !4329, inlinedAt: !4737)
!4739 = !DILocation(line: 111, column: 9, scope: !4722)
!4740 = !DILocation(line: 111, column: 2, scope: !4722)
!4741 = distinct !DISubprogram(name: "release", linkageName: "_ZN7mozilla21ScopedCloseFileTraits7releaseEP8_IO_FILE", scope: !3515, file: !3508, line: 74, type: !3521, isLocal: false, isDefinition: true, scopeLine: 74, flags: DIFlagPrototyped, isOptimized: true, unit: !0, declaration: !3520, retainedNodes: !4742)
!4742 = !{!4743}
!4743 = !DILocalVariable(name: "aFile", arg: 1, scope: !4741, file: !3508, line: 74, type: !3514)
!4744 = !DILocation(line: 74, column: 28, scope: !4741)
!4745 = !DILocation(line: 75, column: 9, scope: !4746)
!4746 = distinct !DILexicalBlock(scope: !4741, file: !3508, line: 75, column: 9)
!4747 = !DILocation(line: 75, column: 9, scope: !4741)
!4748 = !DILocation(line: 76, column: 7, scope: !4749)
!4749 = distinct !DILexicalBlock(scope: !4746, file: !3508, line: 75, column: 16)
!4750 = !DILocation(line: 77, column: 5, scope: !4749)
!4751 = !DILocation(line: 78, column: 3, scope: !4741)
