// Sotirios Dimitrakoulakos

package paradis.assignment2;

class Operation implements Runnable {
	private final int ACCOUNT_ID;
	private final int AMOUNT;
	private final Account account;
	
	Operation(Bank bank, int accountId, int amount) {
		ACCOUNT_ID = accountId;
		AMOUNT = amount;
		account = bank.getAccount(ACCOUNT_ID); // Does not need lock, since getAccount is already sychronized with locks in Bank.java
	}
	
	public int getAccountId() {
		return ACCOUNT_ID;  // Does not need lock, since ID can only be read
	}
	
	// We sychronize the setting of the balance (in case of any problems)
	public void run() {
		sychronized(this){
			int balance = account.getBalance();
			balance = balance + AMOUNT;
			account.setBalance(balance); // Does not need lock, since setBalance is already sychronized with locks in Account.java
		}
	}
}	
