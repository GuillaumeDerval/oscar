package visualisation;

import org.graphstream.graph.Edge;
import org.graphstream.graph.Graph;
import org.graphstream.graph.Node;
import org.graphstream.graph.implementations.AdjacencyListGraph;
import org.graphstream.stream.PipeBase;
import org.graphstream.ui.geom.Point3;
import org.graphstream.ui.layout.Layout;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

/**
 * A layout made to show a forest. Most of the implementation comes from the HierarchicalLayout class
 * in GraphStream (LGPL)
 */
public class ForestLayout extends PipeBase implements Layout {
    final LinkedList<String> roots;
    final Graph internalGraph;

    boolean structureChanged;

    Point3 hi, lo;

    long lastStep;

    int nodeMoved;

    public ForestLayout() {
        roots = new LinkedList<String>();
        internalGraph = new AdjacencyListGraph("hierarchical_layout-intern");
        hi = new Point3();
        lo = new Point3();
    }

    public void setRoots(String... rootsId) {
        roots.clear();

        if (rootsId != null) {
            for (String id : rootsId)
                roots.add(id);
        }
    }

    public void setRoots(List<String> rootsId) {
        roots.clear();

        if (rootsId != null) {
            for (String id : rootsId)
                roots.add(id);
        }
    }

    public void clear() {}

    public void compute() {
        nodeMoved = 0;

        if (structureChanged) {
            structureChanged = false;
            computePositions();
        }

        publishPositions();
        lastStep = System.currentTimeMillis();
    }

    protected void computePositions() {

        LinkedList<Node> roots = new LinkedList<Node>();

        if (this.roots.size() > 0) {
            for (int i = 0; i < this.roots.size(); i++)
                roots.add(internalGraph.getNode(this.roots.get(i)));
        }

        if (roots.size() == 0) {
            int max = internalGraph.getNode(0).getDegree();
            int maxIndex = 0;

            for (int i = 1; i < internalGraph.getNodeCount(); i++)
                if (internalGraph.getNode(i).getDegree() > max) {
                    max = internalGraph.getNode(i).getDegree();
                    maxIndex = i;
                }

            roots.add(internalGraph.getNode(maxIndex));
        }

        ArrayList< LinkedList<Node> > levelContents = new ArrayList<>();
        levelContents.add(new LinkedList<Node>());
        for (int i = 0; i < roots.size(); i++) {
            Node n = roots.get(i);
            levelContents.get(0).add(n);
        }

        do {
            LinkedList<Node> level = new LinkedList<Node>();

            while (roots.size() > 0) {
                Node root = roots.poll();

                for (Edge e : root.getEnteringEdgeSet()) {
                    Node op = e.getOpposite(root);
                    if(!op.hasAttribute("parent")) {
                        level.add(op);
                        op.setAttribute("parent", root);
                    }
                }
            }

            roots.addAll(level);
            levelContents.add(level);
        } while (roots.size() > 0);

        double levelHeight = 400/levelContents.size();
        double levelWidth = 600;
        hi.x = hi.y = Double.MIN_VALUE;
        lo.x = lo.y = Double.MAX_VALUE;

        ListIterator< LinkedList<Node> > levelItr = levelContents.listIterator();
        while(levelItr.hasNext()) {
            int levelIdx = levelItr.nextIndex();
            LinkedList<Node> level = levelItr.next();

            double elementWidth = levelWidth/level.size();
            ListIterator<Node> elemItr = level.listIterator();
            while(elemItr.hasNext())
            {
                int elemIdx = elemItr.nextIndex();
                Node elem = elemItr.next();
                double x = elementWidth*elemIdx + elementWidth/2;
                double y = levelHeight*levelIdx + levelHeight/2;
                elem.setAttribute("x", x);
                elem.setAttribute("y", y);
                if (!elem.hasNumber("oldX") || elem.getNumber("oldX") != x
                        || !elem.hasNumber("oldY") || elem.getNumber("oldY") != y) {
                    elem.setAttribute("oldX", x);
                    elem.setAttribute("oldY", y);
                    elem.addAttribute("changed");
                    nodeMoved++;
                }
                hi.x = Math.max(hi.x, x);
                hi.y = Math.max(hi.y, y);
                lo.x = Math.min(lo.x, x);
                lo.y = Math.min(lo.y, y);
            }
        }
    }

    protected void publishPositions() {
        for (Node n : internalGraph) {
            if (n.hasAttribute("changed")) {
                n.removeAttribute("changed");

                sendNodeAttributeChanged(sourceId, n.getId(), "xyz", null,
                        new double[] { n.getNumber("x"), n.getNumber("y"), 0 });
            }
        }
    }

    public void freezeNode(String id, boolean frozen) {}
    public double getForce() { return 0; }
    public Point3 getHiPoint() { return hi; }
    public long getLastStepTime() { return lastStep; }
    public String getLayoutAlgorithmName() { return "Hierarchical"; }
    public Point3 getLowPoint() { return lo; }
    public int getNodeMovedCount() { return nodeMoved; }
    public double getQuality() { return 0; }
    public double getStabilization() { return 1 - nodeMoved / (double) internalGraph.getNodeCount(); }
    public double getStabilizationLimit() { return 1; }
    public int getSteps() { return 0; }
    public void moveNode(String id, double x, double y, double z) {}
    public void setForce(double value) {}
    public void setQuality(double qualityLevel) {}
    public void setSendNodeInfos(boolean send) {}
    public void setStabilizationLimit(double value) {}
    public void shake() {}

    public void nodeAdded(String sourceId, long timeId, String nodeId) {
        internalGraph.addNode(nodeId);
        structureChanged = true;
    }

    public void nodeRemoved(String sourceId, long timeId, String nodeId) {
        internalGraph.removeNode(nodeId);
        structureChanged = true;
    }

    public void edgeAdded(String sourceId, long timeId, String edgeId,
                          String fromId, String toId, boolean directed) {
        internalGraph.addEdge(edgeId, fromId, toId, directed);
        structureChanged = true;
    }

    public void edgeRemoved(String sourceId, long timeId, String edgeId) {
        internalGraph.removeEdge(edgeId);
        structureChanged = true;
    }

    public void graphCleared(String sourceId, long timeId) {
        internalGraph.clear();
        structureChanged = true;
    }
}