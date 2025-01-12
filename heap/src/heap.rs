use crate::arrays::Array;
use crate::block_headers::{BlockHeader, BlockState};
use crate::blocks::{Block, BlockError, InBlockPtr, LINE_COUNT};
use crate::boxed_values::BoxedValue;
use crate::compactor::{HeapCompactor, LivenessFlag};
use crate::heap_objects::{Header, Markable, Object};
use crate::lists::List;
use crate::memory::{Memory, MemoryError};
use crate::strings::Str;
use crate::tombstones::Tombstone;
use crate::values::Value;

/// GC-ed Heap
pub struct Heap {
    memory: Memory,
    compactor: HeapCompactor,
}

impl Heap {
    pub fn new() -> Self {
        Self {
            memory: Memory::new(),
            compactor: HeapCompactor::new(),
        }
    }

    pub(crate) fn alloc(
        &mut self,
        alloc_size: usize,
    ) -> Result<InBlockPtr, MemoryError> {
        self.memory.alloc(alloc_size)
    }

    pub fn init_gc_cycle(&mut self) {
        self.compactor.init_cycle(&mut self.memory);
    }
    pub fn mark(&mut self, value: &Value) {
        let _ = self.compactor.mark_and_compact(value, &mut self.memory);
    }

    pub fn end_gc_cycle(&mut self) {
        self.compactor.end_cycle(&mut self.memory);
    }

    pub fn unmarked_flag(&self) -> LivenessFlag {
        self.compactor.unmarked_flag()
    }
}
