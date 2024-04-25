// Sotirios Dimitrakoulakos

package paradis.assignment2;

import java.util.ArrayList;
import java.util.List;

class Transaction implements Runnable {
	private List<Operation> operations = new ArrayList<Operation>();
	private boolean closed = false;
	private final Lock lock = new ReentrantLock();  // ReentrantLock which is a mutex (owned by at most one thread) 
													// and a reentrant (can be requuired several times by the same lock)
	
	// We lock the adding and closing of Operations
	public void add(Operation operation) {
        lock.lock();
        try {
            if (!closed) {
                operations.add(operation);
            } else {return;}
        } finally {
            lock.unlock();
        }
	}
	
	public void close() {
        lock.lock();
        try {
            closed = true;
        } finally {
            lock.unlock();
        }
	}
	
	public void run() {
		if (!closed) return;
		
		// Execute the operations.
		for (Operation operation : operations) {
			operation.run();
		}
	}
}	
