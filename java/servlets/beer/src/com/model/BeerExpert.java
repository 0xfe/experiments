package com.model;

import java.util.*;

public class BeerExpert {
  public List<String> getBrands(String color) {
    List<String> brands = new ArrayList<String>();

    if (color.equals("amber")) {
      brands.add("Jack Amber");
      brands.add("Red Moose");
    } else {
      brands.add("Some Pale Ale");
      brands.add("Gout Stout");
    }

    return brands;
  }

  public static void main(String[] args) {
    for (String b : new BeerExpert().getBrands("amber")) {
      System.out.println("Brand: " + b);
    }
  }
}
