import java.util.*;
import java.awt.event.*;
import javax.swing.*;
import java.awt.*;
import java.io.*;

class QuizCard {
  private String question;
  private String answer;

  QuizCard(String q, String a) {
    question = q;
    answer = a;
  }

  String getQuestion() {
    return question;
  }

  String getAnswer() {
    return answer;
  }

  String setQuestion(String q) {
    return question = q;
  }

  String setAnswer(String a) {
    return answer = a;
  }
}

public class Quizzer {
  private JFrame frame;
  private JTextArea question;
  private JTextArea answer;

  private ArrayList<QuizCard> cardList;
  private int currentCard = 0;

  public static void main(String[] args) {
    new Quizzer().go();
  }

  public void go() {
    frame = new JFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    JPanel mainPanel = new JPanel();
    Font bigFont = new Font("sansserif", Font.BOLD, 24);

    question = new JTextArea(6, 20);
    question.setLineWrap(true);
    question.setWrapStyleWord(true);
    question.setFont(bigFont);

    JScrollPane qScroller = new JScrollPane(question);
    qScroller.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    qScroller.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

    answer = new JTextArea(6,20);
    answer.setLineWrap(true);
    answer.setWrapStyleWord(true);
    answer.setFont(bigFont);

    JScrollPane aScroller = new JScrollPane(answer);
    aScroller.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS);
    aScroller.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);

    JButton nextButton = new JButton("Next Card");
    nextButton.addActionListener(new NextCardListener());

    cardList = new ArrayList<QuizCard>();

    JLabel qLabel = new JLabel("Question:");
    JLabel aLabel = new JLabel("Answer:");

    mainPanel.add(qLabel);
    mainPanel.add(qScroller);
    mainPanel.add(aLabel);
    mainPanel.add(aScroller);
    mainPanel.add(nextButton);

    JMenuBar menuBar = new JMenuBar();
    JMenu fileMenu = new JMenu("File");

    JMenuItem newMenuItem = new JMenuItem("New");
    JMenuItem openMenuItem = new JMenuItem("Open");
    JMenuItem saveMenuItem = new JMenuItem("Save");

    newMenuItem.addActionListener(new NewMenuListener());
    openMenuItem.addActionListener(new OpenMenuListener());
    saveMenuItem.addActionListener(new SaveMenuListener());

    fileMenu.add(newMenuItem);
    fileMenu.add(openMenuItem);
    fileMenu.add(saveMenuItem);

    menuBar.add(fileMenu);

    frame.setJMenuBar(menuBar);
    frame.getContentPane().add(BorderLayout.CENTER, mainPanel);
    frame.setSize(500, 600);
    frame.setVisible(true);
  }

  private void clearCard() {
    question.setText("");
    answer.setText("");
    question.requestFocus();
  }

  public class NextCardListener implements ActionListener {
    public void actionPerformed(ActionEvent ev) {
      QuizCard card = new QuizCard(question.getText(), answer.getText());
      cardList.add(card);
      clearCard();
    }
  }

  public class SaveMenuListener implements ActionListener {
    public void actionPerformed(ActionEvent ev) {
      QuizCard card = new QuizCard(question.getText(), answer.getText());
      cardList.add(card);
      
      JFileChooser fileSave = new JFileChooser();
      fileSave.showSaveDialog(frame);
      saveFile(fileSave.getSelectedFile());
    }
  }

  public class NewMenuListener implements ActionListener {
    public void actionPerformed(ActionEvent ev) {
      cardList.clear();
      clearCard();
    }
  }

  public class OpenMenuListener implements ActionListener {
    public void actionPerformed(ActionEvent ev) {
      JFileChooser fileOpen = new JFileChooser();
      fileOpen.showSaveDialog(frame);

      try {
        File f = fileOpen.getSelectedFile();
        BufferedReader reader = new BufferedReader(new FileReader(f));

        String line = null;
        cardList.clear();
        clearCard();

        while ((line = reader.readLine()) != null) {
          makeCard(line);
        }

        reader.close();
      } catch(IOException ex) {
        System.out.println("Can't open file.");
        ex.printStackTrace();
      }
    }
  }

  private void makeCard(String line) {
    String[] result = line.split("/");
    QuizCard card = new QuizCard(result[0], result[1]);
    cardList.add(card);
    System.out.println("Added: " + card.getQuestion() + " / " + card.getAnswer());
  }

  private void saveFile(File file) {
    try { 
      BufferedWriter writer = new BufferedWriter(new FileWriter(file));

      for (QuizCard card : cardList) {
        writer.write(card.getQuestion() + "/");
        writer.write(card.getAnswer() + "\n");
      }

      writer.close();
    } catch (IOException ex) {
      System.out.println("Couldn't write to file.");
      ex.printStackTrace();
    }
  }
}
