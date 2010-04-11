Node* a;
Node* b;

Node* search (Node* cur,
             vector<Node*>& pathA,
             vector<Node*>& pathB,
             vector<Node*>& curPath) {
  if (cur != NULL) {
    curPath.push_back(cur);
    if (cur == a) {
      copy(cur.begin(), cur.end(), pathA.begin());
    }

    if (cur == b) {
      copy(cur.begin(), cur.end(), pathB.begin());
    }

    if (pathA.size() > 0 && pathB.size() > 0) {
      return compute(pathA, pathB);
    }

    Node* answer = search(cur->lchild, pathA, pathB, curPath);
    if (ans != NULL) {
      return ans;
    }

    answer = search(cur->rchild, pathA, pathB, curPath);
    if (ans != NULL) {
      return ans;
    }

    curPath.pop_back();

    return NULL;
  } else return NULL;
}

