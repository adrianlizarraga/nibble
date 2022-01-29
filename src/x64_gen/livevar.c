

typedef struct X64_BBlockLiveInfo {
    BitArray livein;
    BitArray liveout;
    BitArray use;
    BitArray def;
} X64_BBlockLiveInfo;

void X64_init_bblock_usedef(X64_BBlock* bblock, X64_BBlockLiveInfo* info)
{

}

void X64_live_var_analysis(Allocator* arena, size_t num_bblocks, X64_BBlock** bblocks, u32 num_regs)
{
    X64_BBlockLiveInfo* liveinfos = alloc_array(arena, X64_BBlockLiveInfo, num_bblocks, false);

    // Initialize bit arrays for each basic block
    for (size_t i = 0; i < num_bblocks; i++) {
        X64_BBlockLiveInfo* info = liveinfos + i;

        bit_arr_init(&info->livein, arena, num_regs);
        bit_arr_init(&info->liveout, arena, num_regs);
        bit_arr_init(&info->use, arena, num_regs);
        bit_arr_init(&info->def, arena, num_regs);

        // Init def and use
        X64_init_bblock_usedef(bblocks[i], info);
    }
}
