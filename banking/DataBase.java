package banking;

import java.sql.*;

public class DataBase {

    private String url;

    public DataBase(String fileName){
        this.url = "jdbc:sqlite:" + fileName;
    }

    //Подключение к базе данных
    private Connection connect() {
        // SQLite connection string

        Connection conn = null;
        try {
            conn = DriverManager.getConnection(url);
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
        return conn;
    }

    //Создание нового файла БД, если его ещё нет. Для тестов не нужен
    public void createNewDatabase(String fileName) {

        String url = "jdbc:sqlite:" + fileName;

        try (Connection conn = DriverManager.getConnection(url)) {
            if (conn == null) {
                System.out.println("A new database NOT been created.");
            }

        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }

    //Создание таблицы в базе данных
    public void createNewTable() {

        // SQL statement for creating a new table
        String sql = "CREATE TABLE IF NOT EXISTS card (\n"
                + "    id INTEGER PRIMARY KEY ASC,\n"
                + "    number TEXT,\n"
                + "    pin TEXT,\n"
                + "    balance INTEGER DEFAULT 0\n"
                + ");";
//Это создание крутой таблицы с автоинкрементом id, но тест с ней не проходит
//        String sql = "CREATE TABLE IF NOT EXISTS card (\n"
//                + "    id integer PRIMARY KEY ASC,\n"
//                + "    number text NOT NULL,\n"
//                + "    pin text NOT NULL,\n"
//                + "    balance INTEGER DEFAULT 0\n"
//                + ");";

        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement()) {
            // create a new table
            stmt.execute(sql);
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }

    //Вносим данные в таблицу базы
    public void insert(String cardNumber, String cardPin, int balance) {

        String sql = "INSERT INTO card(number,pin,balance) VALUES(?,?,?)";

        try (Connection conn = this.connect();
             PreparedStatement pstmt = conn.prepareStatement(sql)) {
            pstmt.setString(1, cardNumber);
            pstmt.setString(2, cardPin);
            pstmt.setInt(3, balance);
            pstmt.executeUpdate();
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }

    //Просмотр содержимого базы, сделано для удобства тестирования
    public void selectAll() {
        String sql = "SELECT id, number, pin, balance FROM card";

        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                System.out.println(rs.getInt("id") + "\t" +
                        rs.getString("number") + "\t" +
                        rs.getString("pin") + "\t" +
                        rs.getInt("balance"));
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }

    //Получаем номер карты из БД по заданному номеру карты
    public String selectCardNum(String cardNumber) {

        String resultNumber = "";

        String sql = "SELECT number, pin, balance FROM card WHERE number=" + cardNumber;

        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                resultNumber = rs.getString("number");
                System.out.println("selectCard() = " + resultNumber);
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
        return resultNumber;
    }

    //Запрос на получение объекта с номером карты и PIN из БД
    public BankCard selectCard(String cardNumber, String cardPin) {
        BankCard result  = new BankCard();
        String resultNumber = "";
        String resultPin = "";

        String sql = "SELECT number, pin, balance FROM card WHERE number=" + cardNumber
                + " AND pin=" + cardPin;

        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                resultNumber = rs.getString("number");
                resultPin = rs.getString("pin");
                System.out.println("selectCard() = " + resultNumber + "\n pin=" + rs.getString("pin"));
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }

        result.setCardNumber(resultNumber);
        result.setCardPin(resultPin);
        return result;
    }

    //Запрос на получение объекта с номером карты и балансом из БД
    public BankCard selectDestCard(String cardNumber) {
        BankCard result  = new BankCard();
        String resultNumber = "";
        int resultBalance = 0;

        String sql = "SELECT number, balance FROM card WHERE number=" + cardNumber;

        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                resultNumber = rs.getString("number");
                resultBalance = rs.getInt("balance");
                System.out.println("selectCard() = " + resultNumber + "\n bal=" + rs.getInt("balance"));
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }

        result.setCardNumber(resultNumber);
        result.setCardBalance(resultBalance);
        return result;
    }

    //Удаление данных карты из БД
    public void closeAccount(BankCard bankCard){
        int delId = 0;
        String sql = "SELECT id FROM card WHERE number=" + bankCard.getCardNumber();
        System.out.println("запрос на del " + bankCard.getCardNumber());
        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                delId = rs.getInt("id");
                System.out.println("Найден id = " + delId);
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }

        sql = "DELETE FROM card WHERE id = ?";
        System.out.println("try del " + bankCard.getCardNumber() + " id=" + delId);

        try (Connection conn = this.connect();
             PreparedStatement pstmt = conn.prepareStatement(sql)) {

            // set the corresponding param
            pstmt.setInt(1,delId);
            // execute the delete statement
            pstmt.executeUpdate();

        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }

    //Вносим изменение в ячейку balance
    public void addBalance(int balance, BankCard bankCard) {
        int thisId = 0;
        String sql = "SELECT id FROM card WHERE number=" + bankCard.getCardNumber();
        System.out.println("запрос на пополнение " + bankCard.getCardNumber());
        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                thisId = rs.getInt("id");
                System.out.println("Найден id = " + thisId);
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }

        sql = "UPDATE card SET balance = ? WHERE id = ?";

        try (Connection conn = this.connect();
             PreparedStatement pstmt = conn.prepareStatement(sql)) {

            // set the corresponding param
            pstmt.setInt(1, balance);
            pstmt.setInt(2, thisId);
            // update
            pstmt.executeUpdate();
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }

    //Получаем баланс из БД по объекту карты
    public int getBalance(BankCard currentCard) {
        int resultBalance = 0;
        String sql = "SELECT balance FROM card WHERE number=" + currentCard.getCardNumber();

        try (Connection conn = this.connect();
             Statement stmt = conn.createStatement();
             ResultSet rs = stmt.executeQuery(sql)) {

            // loop through the result set
            while (rs.next()) {
                resultBalance = rs.getInt("balance");
            }
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
        return resultBalance;
    }
}