import datetime
import ezdxf
from ezdxf import zoom
import numpy as np

def main():
    # Declare parameters of your dam here
    H = 7.90
    h0 = 0.80
    b = 3.50
    z = 0.50

    m1 = 3.5
    m2 = 2.5
    k = 1.0
    W = 15.00
    D = 7.0
    Th = 5.10
    alfa = 55
    Hg = 5.10

    mi1 = 0.25
    k1 = 0.0002
    
    _2h = 0.0208 * W**(5/4) * D**(1/3)
    _2L = 0.304 * W * (D**0.5)

    d = 0.375
    l0 = 5.0
    # And you're good to go
    
    m = m1 / 1
    he = (382 * D * 1000 * W**2 * (np.sin(alfa * np.pi / 180))**2) / (Hg * 1e9)
    kw = (1 / (m + 0.25)) * (1.35 + 0.585 * np.sqrt(_2L / _2h))
    hw = k * kw * _2h

    Hz = H + hw + he + z

    lambda_val = mi1 * m1 * H
    A = lambda_val + (Hz - H) * m1 + b + Hz * m2
    a0 = (A / m2) - np.sqrt((A / m2)**2 - (H - h0)**2)
    b2 = m2 * (a0 + h0)
    q1 = (k1 / 2) * ((H**2 - (a0 + h0)**2) / (A - b2))
    
    L = d + m1*(Hz-H) + b + (Hz*m2)-l0

    Xmax = b + (Hz*m2) - l0
    
    intervalX = 1.00
    filtrationCurve = [((Hz*m1+b+Hz*m2)-h0*m2,h0)]
    filtrationCurve += [(Hz*m1+b+Hz*m2-l0,h0)]

    while(((filtrationCurve[-1])[0] > 0) and ((filtrationCurve[-1])[1] <= H)):
        x = (filtrationCurve[-1])[0] - intervalX
        if x < 0:
            x = 0
        y = np.sqrt(((H**2 - h0**2)/L)*((Hz*m1+b+Hz*m2-l0)-x) + h0**2)
        filtrationCurve += [(x,y)]
    
    if((filtrationCurve[-1])[1] > H):
        filtrationCurve.pop()
        
    doc = ezdxf.new(dxfversion="R2018",units=6)
    doc.layers.add("waterLevels")  
    doc.layers.add("damStructure")
    doc.layers.add("filtrationCurve")

    damModel = doc.modelspace()

    damStructure = [(0, 0), (Hz*m1, Hz), (Hz*m1+b, Hz), (Hz*m1+b+Hz*m2,0)]
    referenceLevel = [(0,0), (Hz*m1+b+Hz*m2,0)]
    permeableFoundationSoilLevel = [(0,-(Th)), (Hz*m1+b+Hz*m2,-(Th))]

    damModel.add_lwpolyline(damStructure, dxfattribs={"layer": "damStructure"})
    damModel.add_lwpolyline(referenceLevel, dxfattribs={"layer": "damStructure"})
    damModel.add_lwpolyline(permeableFoundationSoilLevel, dxfattribs={"layer": "damStructure"})

    levelH = [(0,H), (H*m1,H)]
    levelH0 = [(Hz*m1+b+Hz*m2,h0), ((Hz*m1+b+Hz*m2)-h0*m2,h0)]
    damModel.add_lwpolyline(levelH, dxfattribs={"layer": "waterLevels"})
    damModel.add_lwpolyline(levelH0, dxfattribs={"layer": "waterLevels"})
    
    damModel.add_lwpolyline(filtrationCurve, dxfattribs={"layer": "filtrationCurve"})
    
    ezdxf.zoom.extents(damModel)
   
    filename = "dam-" + datetime.datetime.now().strftime("%Y-%m-%d_%H_%M_%S") + ".dxf"
    doc.saveas(filename)
    print(f"DXF file '{filename}' created successfully.")
    
if __name__ == "__main__":
    main()
