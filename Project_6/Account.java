// Sotirios Dimitrakoulakos

package paradis.assignment2;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Account {
	// Instance variables.
	private final int ID;
	private int balance;
	private Lock lock = new ReentrantLock(); // ReentrantLock which is a mutex (owned by at most one thread) 
											// and a reentrant (can be requuired several times by the same lock)
	
	// Constructor.
	Account(int id, int balance) {
		ID = id;
		this.balance = balance;
	}
	
	// Instance methods.
	
	public int getId() { // Does not need lock, since ID can only be read
		return ID;
	}
	
	// We lock the reading and writing of the balance field
	public int getBalance() {
		lock.lock();
		try {
			return balance;
		} finally {
			lock.unlock();  
		}
	}
	
	public void setBalance(int balance) {
		lock.lock();
        try {
            this.balance = balance;
        } finally {
            lock.unlock();
        }
	}
}
