// Sotirios Dimitrakoulakos

package paradis.assignment2;

import java.util.List;
import java.util.ArrayList;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

class Bank {
	// Instance variables.
	private final List<Account> accounts = new ArrayList<Account>();
	private final Lock lock = new ReentrantLock(); 	// ReentrantLock which is a mutex (owned by at most one thread) 
													// and a reentrant (can be requuired several times by the same lock)

// We lock the creation and retrieval of Accounts and sychronize their creation (in case of any problems)
	public int newAccount(int balance) {
		sychronized (this) {
			lock.lock();
			try {
				int accountId = accounts.size();
				accounts.add(new Account(accountId, balance));
				return accountId;
			} finally {
				lock.unlock();
			}
		}
	}

	public Account getAccount(int accountId) {
        lock.lock();
        try {
            if (accountId >= 0 && accountId < accounts.size()) {
                return accounts.get(accountId);
            }
            return null;
        } finally {
            lock.unlock();
        }
	}
}