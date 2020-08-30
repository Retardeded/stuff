package banking;

import java.util.Scanner;

import static banking.BankCard.checkSum;

public class Bank {
    Scanner scanner = new Scanner(System.in);
    DataBase dataBase;


    public Bank(String fileName) {
        this.dataBase = new DataBase(fileName);
    }

    //Главное меню
    public void init() {
        int choice;

        do {
            System.out.println("1. Create account");
            System.out.println("2. Log into account");
            System.out.println("0. Exit");
            choice = scanner.nextInt();
            switch (choice) {
                case 0:
                    //Выход из программы
                    exit();
                    break;
                case 1:
                    //Создание карты и pin
                    createCard();
                    break;
                case 2:
                    //Вход в личный кабинет
                    logIntoAccount();
                    break;
                default:
                    System.out.println("ERROR: Не верный номер.");
            }
        } while (choice != 0);
    }

    //Создание карты и пин
    public void createCard(){
        BankCard bankCard = new BankCard();

        System.out.println("\nYour card have been created");
        System.out.println("Your card number:\n" + bankCard.getCardNumber());
        System.out.println("Your card PIN:\n" + bankCard.getCardPin() + "\n");

        //Поместим полученную карту в БД
        dataBase.insert(bankCard.getCardNumber(),bankCard.getCardPin(),0);
//        dataBase.selectAll();
    }

    //Логин в свой аккаунт
    private void logIntoAccount() {
        String inputCardNumber;                       //Введенный номер карты
        String inputCardPin;                          //Введенный pin
        BankCard currentCard;                         //Полученная карта

        scanner.nextLine();  //Очистим сканер
        System.out.println("\nEnter your card number:");
        inputCardNumber = scanner.nextLine();
        System.out.println("Enter your PIN:");
        inputCardPin = scanner.nextLine();
        //Запросим у БД объект по введенным номеру карты и PIN
        currentCard = dataBase.selectCard(inputCardNumber, inputCardPin);
        //Проверим есть ли такая карта и такой PIN
        if(currentCard.getCardNumber().equals(inputCardNumber) || currentCard.getCardPin().equals(inputCardPin)){
            System.out.println("\nYou have successfully logged in!\n");
            operationAcc(currentCard);
        } else {
            System.out.println("\nWrong card number or PIN!\n");
        }
    }

    //Операции в личном кабинете
    private void operationAcc(BankCard currentCard) {
        int choice;

        do {
            System.out.println("1. Balance");
            System.out.println("2. Add income");
            System.out.println("3. Do transfer");
            System.out.println("4. Close account");
            System.out.println("5. Log out");
            System.out.println("0. Exit");
            choice = scanner.nextInt();
            switch (choice) {
                case 0:
                    exit();
                    break;
                case 1:
                    //Запрос баланса
                    viewBalance(currentCard);
                    break;
                case 2:
                    //Добавить денег на счет
                    addDeposit(currentCard);
                    break;
                case 3:
                    //Перевод денег
                    doTransfer(currentCard);
                    break;
                case 4:
                    //Удаление аккаунта
                    closeAccount(currentCard);
                    dataBase.selectAll();
                    return;
                case 5:
                    //Выход из кабинета
                    System.out.println("\nYou have successfully logged out!\n");
                    return;
                default:
                    System.out.println("ERROR: Не верный номер.");
            }
        } while (choice != 0);

    }


    //Запрос баланса
    private void viewBalance(BankCard currentCard) {
        dataBase.getBalance(currentCard);
        System.out.println("\nBalance: " + dataBase.getBalance(currentCard) + "\n");
    }

    //Внесение депозита
    private void addDeposit(BankCard currentCard) {
        System.out.println("\nВносим:");
        dataBase.addBalance(scanner.nextInt() + dataBase.getBalance(currentCard), currentCard);
    }

    //Трансфер
    private void doTransfer(BankCard currentCard) {
        String destCardNum;     //Номер карты получателя
        String destCheckSum;    //Контр сумма номера карты получателя
        int transfer;           //Сумма перевода
        BankCard destCard;      //Объект карта получателя
        scanner.nextLine();     //Очистим сканер
        //Проверим введенный номер на правильность по алгоритму Луна
        do {
            System.out.println("\nКарта получателя");
            destCardNum = scanner.nextLine();

            destCheckSum = checkSum(destCardNum.substring(0, destCardNum.length() - 1));

            if (!String.valueOf(destCardNum.charAt(15)).equals(destCheckSum)) {
                System.out.println("Probably you made mistake in card number. Please try again!\n");
            }

        } while (!String.valueOf(destCardNum.charAt(15)).equals(destCheckSum));

        //Проверим есть ли такая карта в базе
        if(!destCardNum.equals(dataBase.selectCardNum(destCardNum))){
            System.out.println("\nSuch a card does not exist.\n");
            return;
        }

        //Проверим может введен номер уже залогиневшейся карты
        if(destCardNum.equals(currentCard.getCardNumber())){
            System.out.println("\nYou can't transfer money to the same account!\n");
            return;
        }

        //Проверим хватает ли денег на счте для трансфера
        do{
            System.out.println("\nСколько переводим");
            transfer = scanner.nextInt();
            if(transfer > dataBase.getBalance(currentCard)) {
                System.out.println("\nНе хватает денег, пробуем ещё\n");
            }
        } while (transfer > dataBase.getBalance(currentCard));

        //Осуществляем трансфер
        destCard = dataBase.selectDestCard(destCardNum);
        dataBase.addBalance(transfer + dataBase.getBalance(destCard), destCard);
        dataBase.addBalance(dataBase.getBalance(currentCard) - transfer, currentCard);
    }

    //Удаление аккаунта
    private void closeAccount(BankCard currentCard) {
        dataBase.closeAccount(currentCard);
        System.out.println("\nAccount " + currentCard.getCardNumber() + " is deleted...");
    }

    //Безопасный выход
    private void exit() {
        System.out.println("\nBye!");
        System.exit(0);
    }
}